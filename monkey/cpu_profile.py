from com.android.monkeyrunner import MonkeyRunner as mkr
from com.android.monkeyrunner import MonkeyDevice as mkd

import os, sys
sys.path.append(os.getcwd())

import common

# String Constants
cpucfg_pkg = 'br.ufpe.emilianofirmino.cpunuke'
cpustress_pkg = 'br.ufpe.emilianofirmino.cpujudge'

def config_cpu(device, corenum, speed):
    common.open_app(device, cpucfg_pkg)
    mkr.sleep(5)
    for core in range(corenum):
        device.drag((384, 400), (384, 250))  # select number of cpu core
    device.type(str(speed))
    mkr.sleep(5)
    common.press_back(device)

def run_stress(device):
    common.open_app(device, cpustress_pkg)
    mkr.sleep(60)
    device.touch(384, 200, mkd.DOWN_AND_UP) # click run button
    mkr.sleep(10 * 60)
    common.unlock_screen(device)
    mkr.sleep(60)
    common.press_back(60)

if __name__ == "__main__":
    # Estabilish Connection
    d = common.open_dev()
    cpu_core = [1, 2, 3, 4]
    cpu_freq = [384, 486, 594, 702, 810, 918, 1026, 1134, 1242, 1350, 1458, 1512]
    # Run all cpu tests
    for n in cpu_core:
        for f in cpu_freq:
            config_cpu(d, n, f)
            common.start_prof(d)
            run_stress(d)
            common.stop_prof(d)
            mkr.sleep(60)
    # Close Connection
    d.close()
