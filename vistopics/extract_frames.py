def extract_frames_main_func(video_file, pathOut, frame_rate):
    """
    Extracts frames from a video file and saves them to the specified directory.

    Args:
        video_file (str): Path to the video file.
        pathOut (str): Directory to save the extracted frames.
    """
    import os
    import cv2
    import logging

    if not isinstance(frame_rate, int) or frame_rate <= 0:
        raise ValueError("frame_rate must be a positive integer.")

    print(f"Starting extraction for video file: {video_file}")

    # Create the output directory if it doesn't exist
    if not os.path.exists(pathOut):
        os.makedirs(pathOut)
        print(f"Created directory: {pathOut}")

    cap = cv2.VideoCapture(video_file)
    if not cap.isOpened():
        logging.error(f"Error opening video file: {video_file}")
        return

    frame_rate = frame_rate  # Desired frame rate (1 frame per second)
    frame_count = 0

    while True:
        ret, frame = cap.read()
        if not ret:
            print(f"Finished reading frames from video file: {video_file}")
            break
        frame_count += 1
        # Only extract frames at the desired frame rate
        if frame_count % int(cap.get(cv2.CAP_PROP_FPS) / frame_rate) == 0:
            output_file = os.path.join(pathOut, f"frame_{frame_count}.jpg")
            cv2.imwrite(output_file, frame)
            logging.debug(f"Extracted and saved frame {frame_count} to {output_file}")

    cap.release()
    cv2.destroyAllWindows()
    print(f"Released video capture and destroyed all windows for video file: {video_file}")


def list_files(folder):
    """
    Lists all files in the specified folder and its subdirectories.

    Args:
        folder (str): Directory to search for files.

    Returns:
        list: List of file paths.
    """
    import os
    file_list = []
    print(f"Listing files in folder: {folder}")
    for root, dirs, files in os.walk(folder):
        for file in files:
            file_list.append(os.path.join(root, file))
            print(f"Found file: {os.path.join(root, file)}")
    return file_list


def get_existing_subfolders(image_dir):
    """
    Gets the names of existing subfolders in a directory.

    Args:
        image_dir (str): Path to the directory.

    Returns:
        set: Set of subfolder names.
    """
    import os
    import glob

    subfolders = [os.path.basename(f) for f in glob.glob(os.path.join(image_dir, "*")) if os.path.isdir(f)]
    return set(subfolders)


def extract_frames(videofolder, images_folder, frame_rate):
    """
    Processes videos in a folder and extracts frames into subdirectories.

    Args:
        videofolder (str): Path to the folder containing video files.
        images_folder (str): Base directory to save extracted frames.
    """
    import os

    print(f"Starting frame extraction in folder: {videofolder}")
    myvidz = list_files(videofolder)
    if not myvidz:
        print("No video files found in the specified folder.")
    else:
        print(f"Found {len(myvidz)} video(s): {myvidz}")

    existing_subfolders = get_existing_subfolders(images_folder)
    filtered_myvidz = [
        f for f in myvidz
        if os.path.splitext(os.path.basename(f))[0] not in existing_subfolders
    ]

    print(f"Number of .mp4 files to process: {len(filtered_myvidz)}")

    for onevid in filtered_myvidz:
        print(f"Processing video file: {onevid}")
        video_name = os.path.splitext(os.path.basename(onevid))[0]
        pathOut = os.path.join(images_folder, video_name)
        extract_frames_main_func(onevid, pathOut, frame_rate)

    print(f"Completed frame extraction for all videos in folder: {videofolder}")


def count_jpeg_files_in_directory(path):
    """
    Counts the number of JPEG files in a directory and its subdirectories.

    Args:
        path (str): Directory to search for JPEG files.

    Returns:
        int: Number of JPEG files found.
    """
    import os

    count = 0
    for root, dirs, files in os.walk(path):
        print(f"Checking directory: {root}")
        for file in files:
            if file.endswith(".jpg") or file.endswith(".jpeg"):
                print(f"Found .jpg file: {file}")
                count += 1

    print(f"Total .jpg files in directory {path}: {count}")
    return count
