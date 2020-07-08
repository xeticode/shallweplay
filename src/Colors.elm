module Colors exposing (c_amethyst, c_base, c_black, c_border, c_dark, c_darker, c_darkest, c_darkrussianviolet, c_frenchviolet, c_good, c_heliotrope, c_info, c_light, c_lighter, c_lightest, c_mauve, c_persianindigo, c_purple, c_random, c_russianviolet, c_urgent, c_warn, c_white)

import Element exposing (Color, rgb255)


c_darkrussianviolet : Color
c_darkrussianviolet =
    rgb255 16 0 43


c_russianviolet : Color
c_russianviolet =
    rgb255 36 0 70


c_persianindigo : Color
c_persianindigo =
    rgb255 60 9 108


c_purple : Color
c_purple =
    rgb255 90 24 154


c_frenchviolet : Color
c_frenchviolet =
    rgb255 123 44 191


c_amethyst : Color
c_amethyst =
    rgb255 157 78 221


c_heliotrope : Color
c_heliotrope =
    rgb255 199 125 255


c_mauve : Color
c_mauve =
    rgb255 224 170 255


c_black : Color
c_black =
    rgb255 0 0 0


c_border : Color
c_border =
    c_darkrussianviolet


c_darkest : Color
c_darkest =
    c_russianviolet


c_darker : Color
c_darker =
    c_persianindigo


c_dark : Color
c_dark =
    c_purple


c_base : Color
c_base =
    c_frenchviolet


c_light : Color
c_light =
    c_amethyst


c_lighter : Color
c_lighter =
    c_heliotrope


c_lightest : Color
c_lightest =
    c_mauve


c_white : Color
c_white =
    rgb255 248 248 255


c_info : Color
c_info =
    rgb255 91 192 235


c_warn : Color
c_warn =
    rgb255 253 231 76


c_good : Color
c_good =
    rgb255 155 197 61


c_urgent : Color
c_urgent =
    rgb255 229 89 52


c_random : Color
c_random =
    rgb255 250 121 33
