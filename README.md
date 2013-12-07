Introduction
============

This package provides a small framework which allows to utilize your 
muplit core processor. The package can be applied when you can split up the
work into do called partitions which can be processed in parallel.

For more detail please check the documentation under ./doc

The current version as been build for linux on an OpenSuSe 12.3 system.

Contents
========

In you installation directory you will find the following directories:

./doc  		    -- documentation
./src		    -- source code of the framework
./common	    -- common packages
./linux		    -- some os specific stuff
./example	    -- application examples


Installation
============

1) Fetch the source code or the source tar files from github into a directory
   and change into the directory.


2) Currently no system wide installation procedure is available but 
   it is in the backlog for future releases.

   Setup your test environment in the release:
   $ make 
   $ cd example
   $ mkdir yourtest
   $ cd yourtest

   Copy the makefile from particles and modify it.


Examples
========

The application examples are stored under ./examples

./particles  - Simulation of interacting non relativistic particles


