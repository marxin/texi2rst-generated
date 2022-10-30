# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('..')

from baseconf import *

name = 'demo'
project = 'Demo project'
copyright = '2001-2022 Free Software Foundation, Inc.'
authors = 'Martin Liska'

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', f'{name}.tex', project, authors, 'manual'),
]

texinfo_documents = [
  ('index', name, project, authors, None, None, None, True)
]

man_pages = [
    ('demo2', name, 'Demo man page', [authors], 1),
]

set_common(name, globals())

extensions.append('linuxdoc.rstFlatTable')
extensions.append('sphinx_design')
