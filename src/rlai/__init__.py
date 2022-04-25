import os.path

path_to_here = os.path.abspath(os.path.dirname(__file__))
path_to_images = os.path.abspath(os.path.join(path_to_here, os.pardir, os.pardir, "images"))
print(f"path_to_here = {path_to_here}")
print(f"path_to_images = {path_to_images}")
