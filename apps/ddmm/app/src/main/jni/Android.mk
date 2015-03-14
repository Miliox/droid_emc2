LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE    := DDMM
LOCAL_SRC_FILES := DDMM.cpp
LOCAL_LDLIBS    := -llog

include $(BUILD_SHARED_LIBRARY)
