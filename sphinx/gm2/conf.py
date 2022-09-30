# Configuration file for the Sphinx documentation builder.

import sys
sys.path.append('..')

from baseconf import *

name = 'gm2'
project = 'The GNU Modula-2 Compiler'
copyright = '2022 Free Software Foundation, Inc.'
authors = 'Gaius Mulley'

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
latex_documents = [
  ('index', f'{name}.tex', project, authors, 'manual'),
]

texinfo_documents = [
  ('index', name, project, authors, None, None, None, True)
]

tags.add(name)
set_common(name)