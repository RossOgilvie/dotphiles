#1/bin/zsh

theme="row_center"
dir="$HOME/.config/rofi/launchers/misc"

rofi -show window -theme $dir/"$theme" -window-format "({w}) {t}" -no-sort -no-levenshtein-sort
