def get_first_image_and_caption(page_url):
    import requests
    from bs4 import BeautifulSoup
    from urllib.parse import urljoin

    try:
        response = requests.get(page_url, timeout=10)
        response.raise_for_status()
    except Exception as e:
        print(f"Request failed for {page_url}: {e}")
        return None, None

    # Ensure we're working with HTML, not binary
    content_type = response.headers.get("Content-Type", "")
    if "text/html" not in content_type:
        print(f"Skipping non-HTML content from {page_url} (Content-Type: {content_type})")
        return None, None

    try:
        soup = BeautifulSoup(response.text, "html.parser")
    except Exception as e:
        print(f"BeautifulSoup parsing failed for {page_url}: {e}")
        return None, None

    img_tag = soup.find("img")
    if not img_tag:
        return None, None

    img_src = img_tag.get("src")
    if not img_src:
        return None, None

    full_img_url = urljoin(page_url, img_src)

    caption = None
    figure = img_tag.find_parent("figure")
    if figure:
        figcaption = figure.find("figcaption")
        if figcaption:
            caption = figcaption.get_text(strip=True)

    if not caption:
        next_p = img_tag.find_next_sibling("p")
        if next_p:
            caption = next_p.get_text(strip=True)

    return full_img_url, caption



def download_image(url, save_path):
    import requests
    headers = {
        "User-Agent": "Mozilla/5.0 (compatible; VisTopicsBot/1.0; +https://github.com/aysedeniz09/VisTopics)"
    }

    try:
        response = requests.get(url, headers=headers, timeout=10)
        response.raise_for_status()
        with open(save_path, 'wb') as f:
            f.write(response.content)
        return True
    except Exception as e:
        print(f"Exception for {url}: {e}")
        return False


def download_images_from_url(input_csv, output_csv, image_dir):
    import os
    import csv
    import pandas as pd
    from time import sleep
    from random import uniform
    from tqdm import tqdm

    os.makedirs(image_dir, exist_ok=True)

    df = pd.read_csv(input_csv)
    if "index" in df.columns:
        df["index_number"] = df["index"]
    elif "index_number" in df.columns:
        pass  # use as-is
    else:
        df["index_number"] = ["index_" + str(i) for i in range(df.shape[0])]

    with open(output_csv, mode='w', newline='', encoding='utf-8') as csvfile:
        fieldnames = ['index_number', 'url', 'has_caption']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()

        for i in tqdm(range(len(df))):
            row = df.iloc[i]
            index_number = row['index_number']
            url = row['url']

            print(f"Processing {i}: {url}")

            # Direct image URL — no caption
            if any(url.lower().endswith(ext) for ext in [".jpg", ".jpeg", ".png", ".gif", ".webp"]):
                image_url = url
                has_caption = False
            else:
                try:
                    image_url, caption = get_first_image_and_caption(url)
                    if not image_url:
                        print(f"No image found in {url}")
                        continue
                    has_caption = bool(caption and caption.strip())
                except Exception as e:
                    print(f"Skipping {url} — caption scrape failed: {e}")
                    continue

            filename = os.path.join(image_dir, f"{index_number}.jpg")
            success = download_image(image_url, filename)

            if not success:
                print(f"Failed to download {image_url}")
                continue

            writer.writerow({
                'index_number': index_number,
                'url': url,
                'has_caption': has_caption
            })
            csvfile.flush()
            sleep(uniform(0.1, 0.5))

    print(f"Download complete. Output written to {output_csv}")

