# gimp-animation-scripts

A collection of GIMP scripts to help with animation mainly for pixel art.

## Installing

Put `.scm` files that you want inside GIMP scripts folder. You can find
the location at `Edit` > `Preferences` > `Folders` > `Scripts`.

## Layer structure for onion skinning

The image needs to have a proper layer structure for onion skinning. Each
frame should be a layer group consists of at least a background layer. The
following image shows an example of a proper layer structure for onion skinning.

![Layer structure image for onion skinning][layer_structure.png]

## Scripts

### Resized Playback

`Filters` > `Animation` > `Resized Playback`

Most pixel art canvas for video games are small, like 16x16, 16x24, or 32x32.
Animation playback that comes with default GIMP can't be resized. This script
will scale the image for the animation playback (without touching your work).

![Resized Playback image][resized_playback.png]

### Resized Playback with Onion Skinning

`Filters` > `Animation` > `Resized Playback with Onion Skinning`

You can use this script to view onion skinning for each frame.

![Resized Playback with Onion Skinning image][resized_playback_w_onion_skinning.png]

### Onion Skinning

`Filters` > `Animation` > `Onion Skinning`

This script will open a new image for the selected layer group for onion skinning.

Workflow:

1. Select layer group (by selecting itself or its children).
2. Run script.
3. Select mode.
4. A new image will opened. The layer group that you want to edit would be at the topmost.
5. Edit topmost layer group.
6. Use `Merge Onion Skin` script to merge back to the original image.
   
![Onion Skinning image][onion_skinning.png]

### Merge Onion Skin

`Filters` > `Animation` > `Merge Onion Skin`

Merge your edited frame in onion skinning image back to original image.



[layer_structure.png]: https://raw.githubusercontent.com/burhanloey/gimp-animation-scripts/master/screenshots/layer_structure.png
[resized_playback.png]: https://raw.githubusercontent.com/burhanloey/gimp-animation-scripts/master/screenshots/resized_playback.png
[onion_skinning.png]: https://raw.githubusercontent.com/burhanloey/gimp-animation-scripts/master/screenshots/onion_skinning.png
[resized_playback_w_onion_skinning.png]: https://raw.githubusercontent.com/burhanloey/gimp-animation-scripts/master/screenshots/resized_playback_w_onion_skinning.png
