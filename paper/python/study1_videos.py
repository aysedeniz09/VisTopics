### SECTION: Imports ------
import os
import random
import shutil
from vistopics import sample_videos, extract_frames_from_videos, reduce_duplicate_frames, get_caption

### SECTION: Settings ------
INPUT_VIDEO_DIR = "VIDZ"
SAMPLED_VIDEO_DIR = "VIDZSAMPLE"
FRAMES_OUTPUT_DIR = "IMAGES/VIDZSAMPLE"
CAPTIONS_FILE = "OUTPUT/study1_captions.csv"
OPENAI_KEY = os.environ.get("OPENAI_API_KEY", "your-openai-key")
MODEL = "gpt-4o-mini"

### SECTION: Step 1 — Sample Videos ------
# Randomly sample 20% of videos and clean file names
sample_videos(
    input_dir=INPUT_VIDEO_DIR,
    output_dir=SAMPLED_VIDEO_DIR,
    sample_ratio=0.2
)

### SECTION: Step 2 — Extract Frames ------
# Extract frames from sampled videos
extract_frames_from_videos(
    input_dir=SAMPLED_VIDEO_DIR,
    output_dir=FRAMES_OUTPUT_DIR
)

### SECTION: Step 3 — Reduce Duplicate Frames ------
# Run FastDup to remove near-duplicate frames
reduce_duplicate_frames(
    frames_dir=FRAMES_OUTPUT_DIR,
    similarity_threshold=0.8
)

### SECTION: Step 4 — Generate Captions ------
# Generate captions for all frames after duplicate removal
get_caption(
    mykey=OPENAI_KEY,
    path_in=FRAMES_OUTPUT_DIR,
    captions_file=CAPTIONS_FILE,
    model=MODEL
)

print(f"Sampled videos: {SAMPLED_VIDEO_DIR}")
print(f"Frames extracted to: {FRAMES_OUTPUT_DIR}")
print(f"Captions saved to: {CAPTIONS_FILE}")
