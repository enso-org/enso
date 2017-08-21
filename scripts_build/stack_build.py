#!/usr/bin/env python3

from . import utils as utils
import os
import subprocess

shell_path = backend_dir  = utils.prep_path('../shell')

def create_bin_dirs():
    os.makedirs(utils.prep_path('../dist/bin/public/luna'), exist_ok=True)

def build(shell):
    os.chdir(shell)
    subprocess.check_output(['stack', 'build', '--copy-bins'])


def link_main_bin ():
    os.chdir(utils.prep_path('../dist/bin'))
    os.symlink('./public/luna', 'main', target_is_directory=True)

def run():
    create_bin_dirs()
    build(shell_path)
    link_main_bin ()

if __name__ == '__main__':
    run()
