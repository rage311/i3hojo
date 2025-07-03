#!/bin/bash

cabal build && rm ./i3hojo && cp $(cabal list-bin i3hojo) .
