#!/bin/bash
# compare_frames.sh - Detect differences between adjacent frames and identify frames with significant changes
#
# Usage:
#   bash compare_frames.sh <frames_dir> [threshold]
#
# Arguments:
#   frames_dir - Directory containing frame_XXXX.png files
#   threshold  - Change detection threshold (0-100, default: 5)
#                Lower values detect smaller changes
#
# Output:
#   List of frames where the change exceeds the threshold (= timing of significant events)
#   <frames_dir>/changes.json

set -euo pipefail

FRAMES_DIR="${1:?Error: frames_dir is required}"
THRESHOLD="${2:-5}"

if ! command -v ffmpeg &> /dev/null; then
    echo "Error: ffmpeg is not installed"
    exit 1
fi

# Get list of PNG frames
FRAMES=($(ls -1 "${FRAMES_DIR}"/frame_*.png 2>/dev/null | sort))
TOTAL=${#FRAMES[@]}

if [ "$TOTAL" -lt 2 ]; then
    echo "Error: Need at least 2 frames, found $TOTAL"
    exit 1
fi

echo "=== Analyzing frame changes ==="
echo "  Frames:    $TOTAL"
echo "  Threshold: $THRESHOLD%"
echo ""

# Read interval from metadata.json
INTERVAL=1
if [ -f "${FRAMES_DIR}/metadata.json" ]; then
    INTERVAL=$(python3 -c "import json; print(json.load(open('${FRAMES_DIR}/metadata.json'))['frame_interval_sec'])" 2>/dev/null || echo "1")
fi

# Calculate differences between each adjacent frame pair
echo "  Comparing adjacent frames..."
CHANGES="["
FIRST=true
SIGNIFICANT_FRAMES=()

for ((i=0; i<TOTAL-1; i++)); do
    FRAME_A="${FRAMES[$i]}"
    FRAME_B="${FRAMES[$((i+1))]}"

    # Calculate the percentage of differing pixels using ImageMagick's compare
    # Fall back to ffmpeg if ImageMagick is not available
    if command -v compare &> /dev/null; then
        DIFF_PCT=$(compare -metric AE "$FRAME_A" "$FRAME_B" /dev/null 2>&1 || true)
        # Convert pixel count to percentage
        TOTAL_PIXELS=$(identify -format "%[fx:w*h]" "$FRAME_A" 2>/dev/null || echo "1000000")
        CHANGE_PCT=$(echo "$DIFF_PCT $TOTAL_PIXELS" | awk '{printf "%.2f", ($1/$2)*100}')
    else
        # Simple ffmpeg-based diff (using PSNR)
        PSNR=$(ffmpeg -v quiet -i "$FRAME_A" -i "$FRAME_B" -lavfi "psnr" -f null - 2>&1 | grep -oP 'average:\K[\d.]+' || echo "inf")
        if [ "$PSNR" = "inf" ]; then
            CHANGE_PCT="0.00"
        else
            # Lower PSNR = larger difference (above 40 is nearly identical, below 20 is a major change)
            CHANGE_PCT=$(echo "$PSNR" | awk '{if ($1 > 50) printf "0.50"; else if ($1 > 40) printf "2.00"; else if ($1 > 30) printf "10.00"; else if ($1 > 20) printf "30.00"; else printf "60.00"}')
        fi
    fi

    TIMESTAMP=$(echo "$((i+1)) * $INTERVAL" | bc -l 2>/dev/null || echo "$((i+1)) $INTERVAL" | awk '{printf "%.2f", $1 * $2}')
    FRAME_NAME=$(basename "${FRAMES[$((i+1))]}")

    IS_SIGNIFICANT="false"
    if (( $(echo "$CHANGE_PCT > $THRESHOLD" | bc -l 2>/dev/null || echo "$CHANGE_PCT $THRESHOLD" | awk '{print ($1 > $2)}') )); then
        IS_SIGNIFICANT="true"
        SIGNIFICANT_FRAMES+=("$FRAME_NAME (${CHANGE_PCT}% @ ${TIMESTAMP}s)")
    fi

    if [ "$FIRST" = "true" ]; then
        FIRST=false
    else
        CHANGES+=","
    fi

    CHANGES+=$(cat <<ENTRY

  {
    "frame": "$FRAME_NAME",
    "frame_index": $((i+1)),
    "timestamp_sec": $TIMESTAMP,
    "change_percent": $CHANGE_PCT,
    "significant": $IS_SIGNIFICANT
  }
ENTRY
)
done

CHANGES+="
]"

# JSON output
echo "$CHANGES" > "${FRAMES_DIR}/changes.json"

# Display results
echo ""
echo "=== Significant changes detected ==="
if [ ${#SIGNIFICANT_FRAMES[@]} -eq 0 ]; then
    echo "  No significant changes above threshold ($THRESHOLD%)"
else
    for sf in "${SIGNIFICANT_FRAMES[@]}"; do
        echo "  → $sf"
    done
fi

echo ""
echo "  Full results: ${FRAMES_DIR}/changes.json"
echo "  Significant frames are likely where user interactions or state changes occurred."
echo ""
echo "=== Recommended frames to analyze ==="
echo "  Always include: frame_0001.png (initial state)"
if [ ${#SIGNIFICANT_FRAMES[@]} -gt 0 ]; then
    echo "  Key changes:"
    for sf in "${SIGNIFICANT_FRAMES[@]}"; do
        echo "    - $sf"
    done
fi
echo "  Always include: $(basename "${FRAMES[$((TOTAL-1))]}" ) (final state)"
