# prompt = '(Pdb++) '
# highlight = True
# encoding = 'utf-8'
# sticky_by_default = True
# # line_number_color = Color.turquoise
# # filename_color = Color.yellow
# current_line_color = 44
# use_pygments = True
# bg = 'dark'
# colorscheme = None
# editor = '${EDITOR:-vi}'
# truncate_long_lines = True
# exec_if_unfocused = None
# disable_pytest_capturing = True
# def setup(self, pdb): pass

from pdb import DefaultConfig


class Config(DefaultConfig):
    sticky_by_default = True
