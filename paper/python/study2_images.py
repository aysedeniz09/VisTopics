### SECTION: Imports ------
import os
import pandas as pd
from vistopics import download_images_from_url, get_caption

### SECTION: Settings ------
DOWNLOADED_IMAGES_CSV = "OUTPUT/study2_download_log.csv"
IMAGE_DIR = "images_study2"
CAPTIONS_FILE = "OUTPUT/study2_captions.csv"
OPENAI_KEY = os.environ.get("OPENAI_API_KEY", "your-openai-key")
MODEL = "gpt-4o-mini"

### SECTION: Step 0 — Download and Prepare Data ------
# Download CSV from OSF
df = pd.read_csv("https://osf.io/5d4x6/download")

# Add the link=url column
df['link'] = df['url']

# Save the modified CSV locally
INPUT_CSV = "DATA/study2_image_links.csv"
os.makedirs("DATA", exist_ok=True)  # Create DATA directory if it doesn't exist
df.to_csv(INPUT_CSV, index=False)


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

print(f"Data downloaded and prepared: {INPUT_CSV}")
print(f"Images saved to: {IMAGE_DIR}")
print(f"Download log saved to: {DOWNLOADED_IMAGES_CSV}")
print(f"Captions saved to: {CAPTIONS_FILE}")
