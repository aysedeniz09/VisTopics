### SECTION: Imports ------
import os
from vistopics import download_images_from_url, get_caption

### SECTION: Settings ------
INPUT_CSV = "DATA/News_Images_2024_06_01_2024_12_31_NewswhipCollection.csv"
DOWNLOADED_IMAGES_CSV = "OUTPUT/study2_download_log.csv"
IMAGE_DIR = "images_study2"
CAPTIONS_FILE = "OUTPUT/study2_captions.csv"
OPENAI_KEY = os.environ.get("OPENAI_API_KEY", "your-openai-key")
MODEL = "gpt-4o-mini"

### SECTION: Step 1 — Download Images ------
# This will:
# - Read the input CSV
# - Download the first image found on each URL
# - Save download log to DOWNLOADED_IMAGES_CSV
# - Store images in IMAGE_DIR
download_images_from_url(
    input_csv=INPUT_CSV,
    output_csv=DOWNLOADED_IMAGES_CSV,
    image_dir=IMAGE_DIR
)

### SECTION: Step 2 — Generate Captions ------
# This will:
# - Generate captions for all images in IMAGE_DIR
# - Save them to CAPTIONS_FILE
get_caption(
    mykey=OPENAI_KEY,
    path_in=IMAGE_DIR,
    captions_file=CAPTIONS_FILE,
    model=MODEL
)

print(f"Images saved to: {IMAGE_DIR}")
print(f"Download log saved to: {DOWNLOADED_IMAGES_CSV}")
print(f"Captions saved to: {CAPTIONS_FILE}")
