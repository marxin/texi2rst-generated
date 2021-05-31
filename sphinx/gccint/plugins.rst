.. _plugins:

Plugins
-------

.. index:: Plugins

GCC plugins are loadable modules that provide extra features to the
compiler.  Like GCC itself they can be distributed in source and
binary forms.

GCC plugins provide developers with a rich subset of
the GCC API to allow them to extend GCC as they see fit.
Whether it is writing an additional optimization pass,
transforming code, or analyzing information, plugins
can be quite useful.

.. toctree::
  :maxdepth: 2

  plugins-loading
  plugin-api
  plugins-pass
  plugins-gc
  plugins-description
  plugins-attr
  plugins-recording
  plugins-gate
  plugins-tracking
  plugins-building
  loading-plugins
  interacting-with-the-pass-manager
  interacting-with-the-gcc-garbage-collector
  giving-information-about-a-plugin
  registering-custom-attributes-or-pragmas
  recording-information-about-pass-execution
  controlling-which-passes-are-being-run
  keeping-track-of-available-passes
  building-gcc-plugins

