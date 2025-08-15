### SECTION: Imports ------
import os
import random
import shutil
import pandas as pd
from vistopics import video_download, extract_frames_from_videos, reduce_duplicate_frames, get_caption

### SECTION: Settings ------
INPUT_VIDEO_DIR = "VIDZ"
SAMPLED_VIDEO_DIR = "VIDZSAMPLE"
FRAMES_OUTPUT_DIR = "IMAGES/VIDZSAMPLE"
CAPTIONS_FILE = "study1_captions.csv"
OPENAI_KEY = os.environ.get("OPENAI_API_KEY", "your-openai-key")
MODEL = "gpt-4o-mini"

### SECTION: Step 1 — Download Videos ------
# Read CSV from OSF and download videos
df = pd.read_csv("https://osf.io/5k283/download")
df.to_csv("study1_video_links.csv", index=False)

video_download(
    input_df_path="study1_video_links.csv",
    output_df_path="cleaned_videos.csv",
    output_dir="downloaded_videos",
    link_column="youtube_url",
    title_column="Video_Title"
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

print(f"Videos downloaded to: downloaded_videos")
print(f"Sampled videos: {SAMPLED_VIDEO_DIR}")
print(f"Frames extracted to: {FRAMES_OUTPUT_DIR}")
print(f"Captions saved to: {CAPTIONS_FILE}")