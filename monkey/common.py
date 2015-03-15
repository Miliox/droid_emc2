from com.android.monkeyrunner import MonkeyDevice as mkd
from com.android.monkeyrunner import MonkeyRunner as mkr

_ddmm_pkg = 'br.ufpe.emilianofirmino.ddmm'

def open_dev():
    """Estabilish a MonkeyDevice connection to android"""
    return mkr.waitForConnection(1000)

def open_app(device, package, activity = '.MainActivity'):
    """Launch activity on device specified by package[, activity]"""
    app = package + '/' + activity
    device.startActivity(component=app)

def press_back(device):
    """Press back button on device"""
    device.press('KEYCODE_BACK', mkd.DOWN_AND_UP)

def lock_screen(device):
    """Lock device"""
    device.press('KEYCODE_POWER', mkd.DOWN_AND_UP)

def unlock_screen(device):
    """Unlock device"""
    device.wake()
    (x1, x2, y) = (768/2, 50, 1000)
    device.drag((x1,y), (x2,y), duration=1.0, steps=50)

def start_ddmm(device):
    """Start DDMM Profiler"""
    open_app(device, _ddmm_pkg)
    mkr.sleep(2)
    device.touch(20,  200, mkd.DOWN_AND_UP) # check prevent sleep
    device.touch(384, 300, mkd.DOWN_AND_UP) # start ddmm
    mkr.sleep(2)
    press_back(device) # close app

def stop_ddmm(device):
    """Stop DDMM Profiler"""
    open_app(device, _ddmm_pkg)
    mkr.sleep(2)
    device.touch(384, 300, mkd.DOWN_AND_UP) # stop ddmm
    press_back(device) # close app

