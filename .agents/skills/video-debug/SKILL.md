---
name: video-debug
description: >
  A skill that takes screen recording videos of web apps, etc. (MP4, MOV, WebM, etc.) as input,
  extracts and analyzes frames to verify UI behavior, and assists with bug identification and debugging.
  Triggered by requests such as "debug this video", "investigate the issue in this screen recording",
  or "find bugs from this screen capture".
---

# Video Debug Skill

Extract frames from screen recordings and analyze UI behavior chronologically to assist with debugging.

## Prerequisites

- `ffmpeg` must be installed (used for frame extraction)
- Video file must be one of the following formats: MP4, MOV, WebM, AVI, MKV

## Workflow

### 1. Retrieve Video Information

First, check the video metadata using `ffprobe`:

```bash
ffprobe -v quiet -print_format json -show_format -show_streams "<video_file_path>"
```

Information to check:
- Video duration
- Resolution (width x height)
- Frame rate (r_frame_rate)

### 2. Frame Extraction

Extract frames using `scripts/extract_frames.sh`:

```bash
bash <this_skill_directory>/scripts/extract_frames.sh <video_file_path> <output_directory> [fps]
```

**Frame extraction strategy:**

| Video Duration | Recommended fps | Expected Frames | Use Case |
|---------------|----------------|-----------------|----------|
| ~5 sec or less | 2 | ~10 | Short interactions |
| 5–30 sec | 1 | ~30 | Typical operation flows |
| 30 sec–2 min | 0.5 | ~60 | Longer operation sequences |
| Over 2 min | 0.25 | Variable | Extended recordings |

**Important:** Too many frames make analysis difficult. Keep it to around **60 frames maximum**.
For longer videos, lower the fps or ask the user which time range the issue occurs in
and narrow the range using `-ss` and `-t`.

### 3. Frame Analysis

Analyze the extracted frames **in chronological order**. Check from the following perspectives:

#### Tracking UI State Changes
- Timing and order of screen transitions
- Changes before and after button/link clicks
- Form input content and its reflection
- Modal/dialog show/hide behavior
- Loading state transitions

#### Bug Symptom Patterns
- **Layout breakage**: Overlapping elements, overflow, unnatural whitespace
- **Flickering**: Rapid display changes across consecutive frames
- **Unresponsive operations**: Expected changes not occurring after clicks
- **Error displays**: Error messages, red borders, warning icons
- **Blank screens**: Empty areas where content should be displayed
- **Infinite loading**: Spinner displayed for an extended period
- **Z-index issues**: Elements hidden behind other elements
- **Responsive breakage**: Inappropriate layout for the screen size

#### Performance Issue Indicators
- Number of frames between action and response (≈ delay time)
- Groups of frames where the same state persists (possible freeze)

### 4. Analysis Report Output

Summarize results in the following format:

```
## Video Analysis Report

### Overview
- Video: <filename>
- Duration: <seconds>
- Resolution: <width>x<height>
- Extracted frames: <count>

### Operation Flow Summary
1. [0:00 - frame_001] Initial screen displayed
2. [0:02 - frame_005] Button clicked
3. ...

### Detected Issues
#### Issue 1: <Issue summary>
- **Timing**: frame_XXX (approx. X:XX)
- **Symptom**: <What exactly is happening>
- **Affected area**: <Which part of the UI is affected>
- **Estimated cause**: <Possible cause in the code>
- **Suggested fix**: <Specific fix approach>
```

### 5. Cross-referencing with Source Code

If the user also provides source code:

1. Identify the component displayed in the problematic frame
2. Review the source of the relevant component
3. Investigate state management, event handlers, and rendering logic
4. Propose specific code fixes

## Analysis Techniques for Specific Scenarios

### Debugging Animations/Transitions
Extract at a high fps (4–5) and track changes between frames in detail.

### Detailed Analysis of a Specific Segment
```bash
# Extract at high fps for 5 seconds starting at the 3-second mark
ffmpeg -ss 3 -t 5 -i input.mp4 -vf "fps=4" output/frame_%04d.png
```

### Comparison with Screenshots
If the user has a screenshot of "how it should look",
place it side by side with the relevant frame and analyze the differences.

## Notes

- Extract all frame images in PNG format (JPEG degrades UI details)
- Use sequential numbering in frame filenames to preserve chronological order
- Always review multiple frames holistically during analysis; do not judge from a single frame
- If the user provides a description (e.g., "this part looks wrong"), focus analysis on that area
- Be aware that sensitive information (e.g., passwords) may be visible in the video
