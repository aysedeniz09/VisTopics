DEFAULT_PROMPT = (
    "Directly describe with brevity and as brief as possible the scene or characters without any introductory "
    "phrase like 'This image shows', 'In the scene', 'This image depicts' or similar phrases. If there is a text in "
    "the image mention there is a text but do not caption the text, just start describing the scene please. If you "
    "recognize historical figures and current celebrities and politicians in the picture give their full name, but "
    "don't give the whole background about who they are"
)


def encode_image(image_path):
    import base64
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')


def _detect_provider(model):
    """Guess the provider from the model name prefix."""
    model_lower = model.lower()
    if model_lower.startswith("claude"):
        return "anthropic"
    if model_lower.startswith("gpt") or model_lower.startswith("o1") or model_lower.startswith("o3"):
        return "openai"
    raise ValueError(
        f"Could not auto-detect provider for model '{model}'. "
        f"Pass provider explicitly as 'openai' or 'anthropic'."
    )


def _caption_openai(base64_image, api_key, model, prompt):
    import requests

    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {api_key}"
    }
    payload = {
        "model": model,
        "messages": [
            {
                "role": "user",
                "content": [
                    {"type": "text", "text": prompt},
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
            return response_json['choices'][0]['message'].get('content', 'Caption not found').strip()
    except requests.RequestException as e:
        print(f"OpenAI API request failed: {e}")
    return "Failed to get caption"


def _caption_anthropic(base64_image, api_key, model, prompt):
    import requests

    headers = {
        "Content-Type": "application/json",
        "x-api-key": api_key,
        "anthropic-version": "2023-06-01",
    }
    payload = {
        "model": model,
        "max_tokens": 300,
        "messages": [
            {
                "role": "user",
                "content": [
                    {
                        "type": "image",
                        "source": {
                            "type": "base64",
                            "media_type": "image/jpeg",
                            "data": base64_image,
                        },
                    },
                    {"type": "text", "text": prompt},
                ],
            }
        ],
    }

    try:
        response = requests.post("https://api.anthropic.com/v1/messages", headers=headers, json=payload)
        response.raise_for_status()
        response_json = response.json()
        if "content" in response_json and response_json["content"]:
            for block in response_json["content"]:
                if block.get("type") == "text":
                    return block["text"].strip()
    except requests.RequestException as e:
        print(f"Anthropic API request failed: {e}")
    return "Failed to get caption"


_PROVIDER_FUNCS = {
    "openai": _caption_openai,
    "anthropic": _caption_anthropic,
}


def get_caption_main_func(base64_image, api_key, model, prompt=None, provider=None):
    """
    Generates a caption for a single image using the specified model and API key.

    Args:
        base64_image (str): Base64-encoded string of the image.
        api_key (str): API key for the selected provider.
        model (str): Model to use for caption generation (e.g., 'gpt-4o-mini', 'claude-haiku-4-5-20251001').
        prompt (str, optional): Instruction sent to the model. Defaults to DEFAULT_PROMPT.
        provider (str, optional): 'openai' or 'anthropic'. Auto-detected from model name prefix if not given.

    Returns:
        str: Generated caption or an error message.
    """
    custom_prompt = prompt if prompt is not None else DEFAULT_PROMPT

    if provider is None:
        provider = _detect_provider(model)

    if provider not in _PROVIDER_FUNCS:
        raise ValueError(f"Unknown provider '{provider}'. Supported: {list(_PROVIDER_FUNCS.keys())}")

    return _PROVIDER_FUNCS[provider](base64_image, api_key, model, custom_prompt)


def list_files(folder):
    import os
    file_list = []
    for root, dirs, files in os.walk(folder):
        for file in files:
            if file.startswith('.'):
                continue
            file_list.append(os.path.join(root, file))
    return file_list


def get_caption(mykey, path_in, captions_file, model, prompt=None, provider=None):
    """
    Generates captions for images and saves them to a CSV file.

    Args:
        mykey (str): API key for the selected provider.
        path_in (str): Directory containing images to process (searched recursively).
        captions_file (str): Path to the CSV file to save captions.
        model (str): Model to use, e.g. 'gpt-4o-mini' or 'claude-haiku-4-5-20251001'.
        prompt (str, optional): Instruction sent to the model. Defaults to DEFAULT_PROMPT.
        provider (str, optional): 'openai' or 'anthropic'. Auto-detected from model name if not given.
    """
    import csv
    import pandas as pd
    import random
    import time

    all_images = list_files(path_in)

    try:
        df = pd.read_csv(captions_file)
        existing_images = df['image_path'].tolist()
    except FileNotFoundError:
        existing_images = []

    new_images = [img for img in all_images if img not in existing_images]

    with open(captions_file, mode='a', newline='') as file:
        writer = csv.writer(file)

        if not existing_images:
            writer.writerow(['image_path', 'caption'])

        for i, img_path in enumerate(new_images):
            print(f"Processing image: {img_path}")
            time.sleep(random.uniform(1, 5))
            base64_image = encode_image(img_path)
            caption = get_caption_main_func(base64_image, mykey, model, prompt=prompt, provider=provider)

            writer.writerow([img_path, caption])
            file.flush()
            print(f"Caption done for image {i + 1}/{len(new_images)}")