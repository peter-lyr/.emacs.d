# 不再使用(20241014)

import ctypes
import sys
import time

import psutil
import pyautogui

# pip install pyautogui
import win32api
import win32gui
import win32process
from win32con import WM_INPUTLANGCHANGEREQUEST

# 此py给emacs.exe切换输入法用，
# nvim-qt.exe也有一个输入法切换，
# 当emacs.exe切换到nvim-qt.exe时，不再切换
exclude_exes = ["nvim-qt.exe"]


def active_window_process_name():
    try:
        pid = win32process.GetWindowThreadProcessId(win32gui.GetForegroundWindow())
        return psutil.Process(pid[-1]).name().strip()
    except:
        return "xx"


def check_input_method():
    user32 = ctypes.WinDLL("user32", use_last_error=True)
    curr_window = user32.GetForegroundWindow()
    thread_id = user32.GetWindowThreadProcessId(curr_window, 0)
    klid = user32.GetKeyboardLayout(thread_id)
    lid = klid & (2**16 - 1)
    lid_hex = hex(lid)
    if lid_hex == "0x409":  # en
        if sys.argv[1] == "ZH":
            pyautogui.hotkey("win", "space")
    elif lid_hex == "0x804":  # zh
        if sys.argv[1] == "EN":
            pyautogui.hotkey("win", "space")


LANG = {"ZH": 0x0804, "EN": 0x0409}
try:
    hwnd = win32gui.GetForegroundWindow()
    win32api.PostMessage(hwnd, WM_INPUTLANGCHANGEREQUEST, None, LANG[sys.argv[1]])
    time.sleep(0.01)
    if active_window_process_name() not in exclude_exes:
        check_input_method()
except Exception as e:
    print("change_language - Exception:", e)
