def encode_image(image_path):
    import base64
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')


def get_caption_main_func(base64_image, api_key, model):
    """
    Generates a caption for a single image using the specified model and API key.

    Args:
        base64_image (str): Base64-encoded string of the image.
        api_key (str): API key for OpenAI.
        model (str): Model to use for caption generation (e.g., 'gpt-4', 'gpt-3.5-turbo').

    Returns:
        str: Generated caption or an error message.
    """
    import requests

    custom_prompt = (
        "Directly describe with brevity and as brief as possible the scene or characters without any introductory "
        "phrase like 'This image shows', 'In the scene', 'This image depicts' or similar phrases. If there is a text in "
        "the image mention there is a text but do not caption the text, just start describing the scene please. If you "
        "recognize historical figures and current celebrities and politicians in the picture give their full name, but "
        "don't give the whole background about who they are"
    )
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}"
    }
    payload = {
        "model": model,  # Dynamic model parameter
        "messages": [
            {
                "role": "user",
                "content": [
                    {"type": "text", "text": custom_prompt},
                    {"type": "image_url", "image_url": {"url": f"data:image/jpeg;base64,{base64_image}"}}
                ]
            }
        ],
        "max_tokens": 300
    }

    try:
        response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)
        response.raise_for_status()
        response_json = response.json()

        if 'choices' in response_json and response_json['choices'] and 'message' in response_json['choices'][0]:
            caption = response_json['choices'][0]['message'].get('content', 'Caption not found').strip()
            return caption
    except requests.RequestException as e:
        print(f"API request failed: {e}")
    return "Failed to get caption"


def list_files(folder):
    """
    Lists all files in the specified folder and subdirectories.

    Args:
        folder (str): Path to the folder.

    Returns:
        list: List of file paths.
    """
    import os
    file_list = []
    for root, dirs, files in os.walk(folder):
        for file in files:
            file_list.append(os.path.join(root, file))
    return file_list


def get_caption(mykey, path_in, captions_file, model):
    """
    Generates captions for images and saves them to a CSV file.

    Args:
        mykey (str): API key for the captioning service.
        path_in (str): Directory containing images to process.
        captions_file (str): Path to the CSV file to save captions.
        model (str): Model to use for caption generation (e.g., 'gpt-4', 'gpt-3.5-turbo').
    """
    import csv
    import os
    import pandas as pd
    import random
    import time

    # List all images in the input directory
    all_images = list_files(path_in)

    # Load existing captions if the file exists
    try:
        df = pd.read_csv(captions_file)
        existing_images = df['image_path'].tolist()
    except FileNotFoundError:
        existing_images = []

    # Filter out images that already have captions
    new_images = [img for img in all_images if img not in existing_images]

    # Open the captions file for appending
    with open(captions_file, mode='a', newline='') as file:
        writer = csv.writer(file)

        # Write header if the file is new
        if not existing_images:
            writer.writerow(['image_path', 'caption'])

        # Process each new image
        for i, img_path in enumerate(new_images):
            print(f"Processing image: {img_path}")
            time.sleep(random.uniform(1, 5))  # Random delay to avoid API rate limits
            base64_image = encode_image(img_path)
            caption = get_caption_main_func(base64_image, mykey, model)

            # Write the current row to the CSV file
            writer.writerow([img_path, caption])
            file.flush()  # Ensure data is written to disk
            print(f"Caption done for image {i + 1}/{len(new_images)}")
