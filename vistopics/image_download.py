import os
import csv
import pandas as pd
from time import sleep
from random import uniform
from urllib.parse import urlparse, urljoin
from tqdm import tqdm
import requests


def get_first_image_and_caption(page_url):
    from bs4 import BeautifulSoup

    headers = {
        "User-Agent": (
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
            "(KHTML, like Gecko) Chrome/120.0 Safari/537.36"
        )
    }

    try:
        response = requests.get(page_url, headers=headers, timeout=10)
        response.raise_for_status()
    except Exception as e:
        print(f"Request failed for {page_url}: {e}")
        return None, None

    content_type = response.headers.get("Content-Type", "")
    if "text/html" not in content_type:
        print(f"Skipping non-HTML content from {page_url} (Content-Type: {content_type})")
        return None, None

    try:
        soup = BeautifulSoup(response.text, "html.parser")
    except Exception as e:
        print(f"BeautifulSoup parsing failed for {page_url}: {e}")
        return None, None

    # prefer og:image (the article's real editorial/social-share image) over
    # a naive first-<img> scan, which can grab ads, promos, or lazy-load
    # placeholders instead of the actual content image
    og_image = soup.find("meta", property="og:image")
    if og_image and og_image.get("content"):
        return urljoin(page_url, og_image["content"]), None

    img_tag = soup.find("img")
    if not img_tag:
        return None, None

    img_src = img_tag.get("src")
    if not img_src or img_src.startswith("data:"):
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


def download_image(url, save_path, referer=None):
    headers = {
        "User-Agent": "Mozilla/5.0 (compatible; VisTopicsBot/1.0; +https://github.com/aysedeniz09/VisTopics)"
    }
    if referer:
        headers["Referer"] = referer

    try:
        response = requests.get(url, headers=headers, timeout=10)
        response.raise_for_status()
        with open(save_path, 'wb') as f:
            f.write(response.content)
        return True
    except Exception as e:
        print(f"Exception for {url}: {e}")
        return False


def _is_direct_image_url(url):
    """Check the URL's path (not query string) for an image extension."""
    path = urlparse(url).path
    return any(path.lower().endswith(ext) for ext in [".jpg", ".jpeg", ".png", ".gif", ".webp"])


def download_images_from_url(input_csv, output_csv, image_dir, url_column="url",
                              index_column=None, use_referer=False):
    """
    Parameters
    ----------
    input_csv : str
        Path to input CSV.
    output_csv : str
        Path to write the download log.
    image_dir : str
        Directory to save images into.
    url_column : str, default "url"
        Name of the column containing the URL to download (can be a direct
        image link OR an article page URL to scrape for an image).
    index_column : str, optional
        Name of the column to use as the unique identifier for each row's
        filename. If not provided, falls back to an "index"/"index_number"
        column if present, otherwise auto-generates one.
    use_referer : bool, default False
        If True, sets the Referer header to the URL's own domain when
        downloading -- helps with CDNs that use hotlink protection.
    """
    os.makedirs(image_dir, exist_ok=True)

    df = pd.read_csv(input_csv)

    if index_column and index_column in df.columns:
        df["index_number"] = df[index_column]
    elif "index" in df.columns:
        df["index_number"] = df["index"]
    elif "index_number" in df.columns:
        pass
    else:
        df["index_number"] = ["index_" + str(i) for i in range(df.shape[0])]

    if url_column not in df.columns:
        raise ValueError(f"Column '{url_column}' not found in {input_csv}. "
                          f"Available columns: {df.columns.tolist()}")

    with open(output_csv, mode='w', newline='', encoding='utf-8') as csvfile:
        fieldnames = ['index_number', 'url', 'has_caption']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()

        for i in tqdm(range(len(df))):
            row = df.iloc[i]
            index_number = row['index_number']
            url = row[url_column]

            print(f"Processing {i}: {url}")

            if _is_direct_image_url(url):
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

            referer = f"https://{urlparse(url).netloc}/" if use_referer else None

            filename = os.path.join(image_dir, f"{index_number}.jpg")
            success = download_image(image_url, filename, referer=referer)

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