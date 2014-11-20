__author__ = 'E341194'

import numpy as np
from PIL import Image


def add_alpha(image_file):
    im = Image.open(image_file)
    im.putalpha(1)
    data = np.array(im)
    w, h, rgba = data.shape
    for i in range(w):
        for j in range(h):
            if data[i, j, 0] == 0 and data[i, j, 1] == 0 and data[i, j, 2] == 0:
                data[i, j] = np.array([0, 0, 0, 0])
    return Image.fromarray(data, 'RGBA')