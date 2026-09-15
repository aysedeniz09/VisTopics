from setuptools import setup, find_packages

setup(
    name="vistopics",
    version="0.1.9",
    description="A package for video and image processing with captioning capabilities",
    long_description=open("README.md").read(),
    long_description_content_type="text/markdown",
    author="Ayse D. Lokmanoglu & Dror Walter",
    author_email="alokman@bu.edu",
    url="https://github.com/aysedeniz09/VisTopics",
    license="MIT",
    packages=find_packages(),
    install_requires=[
        "openai>=1.50.0",
        "opencv-python>=4.9.0,<4.10",
        "opencv-python-headless>=4.9.0,<4.10",
        "pandas>=2.0.3,<2.2",
        "requests>=2.28.0",
        "yt-dlp>=2024.12.6",
        "gradio>=3.36.0",
        "aiofiles>=23.0",
        "pydantic>=2.8",
        "urllib3>=1.26",
        "beautifulsoup4>=4.12"
    ],
    extras_require={
        "fastdup": ["fastdup>=2.15", "numpy~=1.23.0"]
    },
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.9",
)
