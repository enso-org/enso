# Engine benchmarks

This directory contains a python script `bench_download.py` for downloading
Engine benchmark results from GitHub and `Engine_Benchs` Enso project for
analysing the downloaded data.

Dependencies for `bench_download.py`:

- python >= 3.7
- `pandas` and `jinja2`
  - Install, e.g., by `pip install pandas jinja2`
- GH CLI utility
  - Install either from https://cli.github.com/ or with
    `sudo apt-get install gh`

Check `bench_download -h` for documentation and usage. Ensure that your
`/usr/bin/env python` links to Python version at least 3.7.

One can also analyze the benchmarks in Enso IDE by running
`bench_download.py --create-csv` and then running `Engine_Benchs` project. The
created CSV is pasted into `Engine_Benchs/data` directory by default.
