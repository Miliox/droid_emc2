#! /bin/bash
#
# ddmm-util.sh
# Copyright (C) 2015 Emiliano Firmino <emiliano.firmino@gmail.com>
#
# Distributed under terms of the MIT license.
#

copylogs() {
    adb shell ls /sdcard/*.log | tr '\r' ' ' | xargs -n1 adb pull
}

purgelogs() {
    adb shell ls /sdcard/*.log | tr '\r' ' ' | xargs -n1 adb shell rm
}
