# Paper Python Scripts

This folder contains Python scripts for preprocessing, caption generation, and other tasks used in our paper.

## Data Access
Some scripts require datasets stored on OSF:  
https://osf.io/vhdaj/

Please download the relevant datasets before running these scripts.

## Contents
- `study1_videos.py` — Samples videos, extracts frames, reduces duplicates with FastDup, and generates captions using `vistopics` (Study 1)
- `study2_images.py` — Scrapes article pages for images, downloads them, and generates captions using `vistopics` (Study 2)

## Requirements
- Python ≥ 3.10
- Install the core package and dependencies:
```bash
  pip install vistopics
```
- If using FastDup-based duplicate reduction:
```bash
pip install vistopics[fastdup]
```