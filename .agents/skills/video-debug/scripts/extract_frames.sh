#!/bin/bash
# extract_frames.sh - Helper script to extract frames from a video
#
# Usage:
#   bash extract_frames.sh <video_path> <output_dir> [fps] [start_time] [duration]
#
# Arguments:
#   video_path  - Path to the input video file
#   output_dir  - Output directory for extracted frame images
#   fps         - Extraction frame rate (default: auto - determined automatically based on video duration)
#   start_time  - Extraction start time (e.g., 3, 1:30) (omitted: start of video)
#   duration    - Duration to extract (seconds) (omitted: until end of video)
#
# Output:
#   <output_dir>/frame_0001.png, frame_0002.png, ...
#   <output_dir>/metadata.json  (video metadata)

set -euo pipefail

VIDEO_PATH="${1:?Error: video_path is required}"
OUTPUT_DIR="${2:?Error: output_dir is required}"
FPS="${3:-auto}"
START_TIME="${4:-}"
DURATION="${5:-}"

# Check if ffmpeg / ffprobe are installed
if ! command -v ffmpeg &> /dev/null; then
    echo "Error: ffmpeg is not installed. Install with: brew install ffmpeg (macOS) or apt install ffmpeg (Linux)"
    exit 1
fi

if ! command -v ffprobe &> /dev/null; then
    echo "Error: ffprobe is not installed (usually comes with ffmpeg)"
    exit 1
fi

# Check if input file exists
if [ ! -f "$VIDEO_PATH" ]; then
    echo "Error: Video file not found: $VIDEO_PATH"
    exit 1
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Retrieve video metadata
echo "=== Analyzing video ==="
VIDEO_DURATION=$(ffprobe -v quiet -show_entries format=duration -of csv=p=0 "$VIDEO_PATH" | tr -d '[:space:]')
VIDEO_WIDTH=$(ffprobe -v quiet -select_streams v:0 -show_entries stream=width -of csv=p=0 "$VIDEO_PATH" | tr -d '[:space:]')
VIDEO_HEIGHT=$(ffprobe -v quiet -select_streams v:0 -show_entries stream=height -of csv=p=0 "$VIDEO_PATH" | tr -d '[:space:]')
VIDEO_FPS=$(ffprobe -v quiet -select_streams v:0 -show_entries stream=r_frame_rate -of csv=p=0 "$VIDEO_PATH" | tr -d '[:space:]')
VIDEO_CODEC=$(ffprobe -v quiet -select_streams v:0 -show_entries stream=codec_name -of csv=p=0 "$VIDEO_PATH" | tr -d '[:space:]')

echo "  File:       $(basename "$VIDEO_PATH")"
echo "  Duration:   ${VIDEO_DURATION}s"
echo "  Resolution: ${VIDEO_WIDTH}x${VIDEO_HEIGHT}"
echo "  Frame rate: ${VIDEO_FPS}"
echo "  Codec:      ${VIDEO_CODEC}"

# Automatically determine FPS
if [ "$FPS" = "auto" ]; then
    # Use bc for decimal support (fall back to awk if unavailable)
    DURATION_INT=$(printf "%.0f" "$VIDEO_DURATION" 2>/dev/null || echo "$VIDEO_DURATION" | awk '{printf "%d", $1}')

    if [ "$DURATION_INT" -le 5 ]; then
        FPS=2
    elif [ "$DURATION_INT" -le 30 ]; then
        FPS=1
    elif [ "$DURATION_INT" -le 120 ]; then
        FPS=0.5
    else
        FPS=0.25
    fi
    echo "  Auto FPS:   $FPS (based on duration)"
fi

# Calculate estimated frame count
EFFECTIVE_DURATION="$VIDEO_DURATION"
if [ -n "$DURATION" ]; then
    EFFECTIVE_DURATION="$DURATION"
fi
EST_FRAMES=$(echo "$EFFECTIVE_DURATION * $FPS" | bc 2>/dev/null || echo "$EFFECTIVE_DURATION $FPS" | awk '{printf "%d", $1 * $2}')
echo "  Est frames: ~${EST_FRAMES}"

# Warning if too many frames
if [ "$EST_FRAMES" -gt 60 ] 2>/dev/null; then
    echo ""
    echo "  WARNING: Estimated frame count ($EST_FRAMES) exceeds 60."
    echo "  Consider using a lower FPS or narrowing the time range."
    echo ""
fi

# Build ffmpeg command
FFMPEG_ARGS=()

if [ -n "$START_TIME" ]; then
    FFMPEG_ARGS+=(-ss "$START_TIME")
fi

FFMPEG_ARGS+=(-i "$VIDEO_PATH")

if [ -n "$DURATION" ]; then
    FFMPEG_ARGS+=(-t "$DURATION")
fi

# Resize if resolution is very large (down to 1920px width max)
SCALE_FILTER=""
if [ "$VIDEO_WIDTH" -gt 1920 ] 2>/dev/null; then
    SCALE_FILTER="scale=1920:-2,"
    echo "  Scaling:    down to 1920px width (original: ${VIDEO_WIDTH}px)"
fi

FFMPEG_ARGS+=(-vf "${SCALE_FILTER}fps=${FPS}")
FFMPEG_ARGS+=(-frame_pts 1)
FFMPEG_ARGS+=("${OUTPUT_DIR}/frame_%04d.png")

# Execute frame extraction
echo ""
echo "=== Extracting frames ==="
ffmpeg -v warning "${FFMPEG_ARGS[@]}"

# Check actual frame count
ACTUAL_FRAMES=$(ls -1 "${OUTPUT_DIR}"/frame_*.png 2>/dev/null | wc -l | tr -d '[:space:]')
echo "  Extracted:  ${ACTUAL_FRAMES} frames"

# Output metadata JSON
cat > "${OUTPUT_DIR}/metadata.json" <<EOF
{
  "source_video": "$(basename "$VIDEO_PATH")",
  "video_duration_sec": $VIDEO_DURATION,
  "video_resolution": "${VIDEO_WIDTH}x${VIDEO_HEIGHT}",
  "video_fps": "$VIDEO_FPS",
  "video_codec": "$VIDEO_CODEC",
  "extraction_fps": $FPS,
  "start_time": "${START_TIME:-0}",
  "duration": "${DURATION:-$VIDEO_DURATION}",
  "total_frames_extracted": $ACTUAL_FRAMES,
  "frame_interval_sec": $(echo "1 / $FPS" | bc -l 2>/dev/null || echo "1 $FPS" | awk '{printf "%.2f", $1 / $2}'),
  "output_format": "PNG"
}
EOF

echo ""
echo "=== Done ==="
echo "  Frames:   ${OUTPUT_DIR}/frame_*.png"
echo "  Metadata: ${OUTPUT_DIR}/metadata.json"
echo ""

# Output frame list (for analysis)
echo "=== Frame timeline ==="
INTERVAL=$(echo "1 / $FPS" | bc -l 2>/dev/null || echo "1 $FPS" | awk '{printf "%.2f", $1 / $2}')
IDX=0
for f in "${OUTPUT_DIR}"/frame_*.png; do
    TIMESTAMP=$(echo "$IDX * $INTERVAL" | bc -l 2>/dev/null || echo "$IDX $INTERVAL" | awk '{printf "%.2f", $1 * $2}')
    # Convert to mm:ss format
    MINS=$(printf "%.0f" $(echo "$TIMESTAMP / 60" | bc -l 2>/dev/null || echo "$TIMESTAMP" | awk '{printf "%d", $1/60}'))
    SECS=$(printf "%05.2f" $(echo "$TIMESTAMP - $MINS * 60" | bc -l 2>/dev/null || echo "$TIMESTAMP $MINS" | awk '{printf "%.2f", $1 - $2*60}'))
    printf "  %s → %d:%s\n" "$(basename "$f")" "$MINS" "$SECS"
    IDX=$((IDX + 1))
done
