def count_jpeg_files_in_directory(path):
    import os
    count = 0
    for root, dirs, files in os.walk(path):
        print(f"Checking directory: {root}")
        for file in files:
            if file.endswith(".jpg"):
                count += 1
    print(f"Total .jpg files in directory {path}: {count}")
    return count

def limiting_frames(path, output_file, ccthreshold):
    """
    Reduces duplicate frames using FastDup and saves the reduced frame list to a CSV file.

    Args:
        path (str): Path to the directory containing subfolders with images.
        output_file (str): Name of the CSV file to save the reduced frame list.
        ccthreshold (float): Correlation threshold for FastDup to consider frames as duplicates.
    """

    try:
        import fastdup
        import numpy as np
    except ImportError:
        raise ImportError(
            "The 'fastdup' package is required for this function. "
            "Install it with: pip install fastdup numpy~=1.23.0"
        )

    import subprocess
    import sys
    import os

    import pandas as pd
    from glob import glob

    subfolders = glob(f"{path}/*", recursive=True)
    print(f"Length of subfolders folder: {len(subfolders)}")
    counter = 0
    MAIN_original = []
    MAIN_duplicates = []

    for eachfolder in subfolders:
        try:
            print(f"Processing folder: {eachfolder}")
            counter += 1
            print(f"Folder count: {counter}")
            jpeg_count = count_jpeg_files_in_directory(eachfolder)
            print(f"Number of Images count: {jpeg_count}")
            tempfolder = "./temp/" + eachfolder.split("/")[-1]
            fd = fastdup.create(input_dir=eachfolder, work_dir='./temp')
            try:
                print(f"Running Fastdup with ccthreshold={ccthreshold} for {jpeg_count} images...")
                fd.run(ccthreshold=ccthreshold, num_images=jpeg_count, overwrite=True, verbose=False)
                print("Fastdup run completed.")
            except Exception as e:
                print(f"Error running Fastdup: {str(e)}")
                continue

            print("Finding top components...")
            top_components = fastdup.find_top_components('./temp')
            fd = fastdup.delete_components(top_components, None, how='one', dry_run=False)
            print("Top components deleted.")
            jpeg_count = count_jpeg_files_in_directory(eachfolder)
            print(f"Number of Images after fastdup count: {jpeg_count}")
            print("Reading connected components table...")
            comp_table = pd.read_csv("temp/connected_components.csv")
            dups_record_temp = pd.DataFrame(columns=['orig', 'duplicates'])
            for j in set(comp_table['component_id']):
                print(f"Processing component ID: {j}")
                temp_comp_table = comp_table[comp_table['component_id'] == j]
                temp_list = list(temp_comp_table['__id'])
                if len(temp_list) > 1:
                    min_id = min(temp_list)
                    temp_list.pop(temp_list.index(min_id))
                    dup_list = []
                    conversion_table = pd.read_csv("temp/atrain_features.dat.csv")
                    for k in temp_list:
                        dup_list.append(conversion_table[conversion_table['index'] == k]['filename'].values[0])
                    min_id_file_name = conversion_table[conversion_table['index'] == min_id]['filename'].values[0]
                    orig_list = [min_id_file_name] * len(dup_list)
                    MAIN_original.extend(orig_list)
                    MAIN_duplicates.extend(dup_list)

        except Exception as e:
            print(f'Error processing folder {eachfolder}: {str(e)}')

    print("Creating master reduced frame list...")
    os.makedirs("temp", exist_ok=True)
    master_reduced_frame_list = pd.DataFrame({'ORIGINAL': MAIN_original, 'DUPLICATES': MAIN_duplicates})
    master_reduced_frame_list.to_csv(output_file, index=False)
    print(f"Master reduced frame list saved to {output_file}.")
