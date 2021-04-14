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

  How can we load plugins. <plugins-loading>
  The APIs for plugins. <plugin-api>
  How a plugin interact with the pass manager. <plugins-pass>
  How a plugin Interact with GCC Garbage Collector. <plugins-gc>
  Giving information about a plugin itself. <plugins-description>
  Registering custom attributes or pragmas. <plugins-attr>
  Recording information about pass execution. <plugins-recording>
  Controlling which passes are being run. <plugins-gate>
  Keeping track of available passes. <plugins-tracking>
  How can we build a plugin. <plugins-building>

.. toctree::

  loading-plugins
  plugin-api
  interacting-with-the-pass-manager
  interacting-with-the-gcc-garbage-collector
  giving-information-about-a-plugin
  registering-custom-attributes-or-pragmas
  recording-information-about-pass-execution
  controlling-which-passes-are-being-run
  keeping-track-of-available-passes
  building-gcc-plugins

