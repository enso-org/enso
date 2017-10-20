#!/usr/bin/env python3

from distutils import dir_util
from utils import prep_path, working_directory
import os
import subprocess


shell_path = prep_path('../shell')
backend_dir = prep_path('../shell')


def create_dirs():
    os.makedirs(prep_path('../dist/bin/public/luna'), exist_ok=True)
    os.makedirs(prep_path('../dist/config/env'), exist_ok=True)


def build(shell):
    with working_directory(shell):
        subprocess.check_output(['stack', 'build', '--copy-bins'])


def link_main_bin ():
    with working_directory(prep_path('../dist/bin')):
        os.symlink('./public/luna', 'main', target_is_directory=True)


def copy_stdlib():
    dir_util.copy_tree()


def run():
    create_dirs()
    build(shell_path)
    link_main_bin()


if __name__ == '__main__':
    run()
