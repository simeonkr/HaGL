#!/usr/bin/python3

from glob import glob
from os.path import basename, normpath, splitext
import subprocess
from parse import parse


INPUT_DIR = "output/images"
OUTPUT_DIR = "doc/images"
MAX_FRAMES = 200
DELETE_INPUTS = False

for image in glob(INPUT_DIR + "/*.ppm"):
    out_image = OUTPUT_DIR + "/" + basename(splitext(image)[0]) + ".png"
    subprocess.run(["convert", image, out_image])
    if DELETE_INPUTS:
        subprocess.run(["rm", image])


def frame_key(s):
    (base, num) = parse('{}{:d}.ppm', s)
    return (base, int(num))

for frame_dir in glob(INPUT_DIR + "/**/"):
    frames = sorted(glob(frame_dir + "/*.ppm"), key=frame_key)
    # remove first and last frames as they are usually junk
    frames = frames[2:-1][:MAX_FRAMES]

    out_image = OUTPUT_DIR + "/" + basename(normpath(frame_dir)) + ".gif"
    subprocess.run(["convert", 
                    "-delay", "1x60", "-resize", "50%", 
                    "-loop", "0", "-coalesce"] 
                    + frames + [out_image])
    if DELETE_INPUTS:
        subprocess.run(["rm", "-r", frame_dir])
