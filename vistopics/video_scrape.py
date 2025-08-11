def video_download(input_df_path, output_df_path, output_dir, link_column, title_column):
    """
    Cleans video titles, removes duplicates, and downloads videos.
    Args:
        input_df_path (str): Path to the input CSV file.
        output_df_path (str): Path to save the cleaned CSV.
        output_dir (str): Directory to save downloaded videos.
        title_column (str): Name of the column containing video titles.
    """
    import pandas as pd
    import os
    import re
    import time
    from random import randint
    from time import sleep
    import yt_dlp
    import datetime

    original_dir = os.getcwd()

    def clean_title(title):
        """Cleans a title for safe file naming."""
        if pd.isna(title):
            return "no_title"
        title = str(title)
        title = re.sub(r'[\\/*?:"<>|]', "", title)
        title = re.sub(r'[^a-zA-Z0-9\s]', '', title)
        return title.replace(" ", "_").lower()

    # Read input CSV
    df = pd.read_csv(input_df_path)

    # Ensure master_index column exists
    if 'master_index' not in df.columns:
        df['master_index'] = range(len(df))

    # Use the provided title column to create 'cleaned_title'
    if title_column not in df.columns:
        raise KeyError(f"The specified column '{title_column}' does not exist in the input CSV.")
    df['cleaned_title'] = df[title_column].apply(clean_title)

    # Save to output CSV including master_index
    df.to_csv(output_df_path, index=False)

    # Remove duplicates based on links
    df = df.drop_duplicates(subset=link_column)
    total_videos = len(df)
    print(f"Preparing to download {total_videos} videos...")

    os.makedirs(output_dir, exist_ok=True)
    os.chdir(output_dir)

    # Download videos with countdown
    for count, (idx, row) in enumerate(df.iterrows(), start=1):
        url = row[link_column]
        filename = f"{row.get('master_index', idx)}-{row['cleaned_title']}.%(ext)s"
        print(f"[{count}/{total_videos}] Downloading: {filename}")
        try:
            with yt_dlp.YoutubeDL({'outtmpl': filename}) as ydl:
                ydl.download([url])
            sleep(randint(1, 5))
        except Exception as e:
            print(f"Error downloading {url}: {e}")

    # return to original directory
    os.chdir(original_dir)
    print(f"Returned to original directory: {original_dir}")
