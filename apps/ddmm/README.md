# Droid Digital MultiMeter (DDMM)

DDMM is an application monitor and log battery, cpu and network.

The recorded logs enable energy consumption analysis and track
hot energy spots.

## Setup

```
# create local.properties
touch local.properties
# configure sdk location
echo sdk.dir=${ANDROID_HOME} >> local.properties
# configure ndk location
echo ndk.dir=#{ANDROID_NDK} >> local.properties
```

## Build

```
# to build debug version
$ ./gradlew clean assembleDebug

# to build release version
$ ./gradlew clean assembleRelease
```

## Deploy

```
# to install debug version
$ ./grandle installDebug

# to install release version
$ adb install app/build/outputs/apk/app-release-unsigned.apk
```

## Run Application

1. Apps Menu
2. Open DroidDMMActivity
3. Check "Prevent Device Sleep"
4. Click "Start Profiler" Button
5. Press HOME key
6. Do Stuff!!!
7. Open DroidDMMActivity
8. Click "Stop Profiler" Button
9. Press Back to Close Application

## Collect Recorded Logs

```
# copy from device sdcard
adb shell ls /sdcard/*.log | tr '\r' ' ' | xargs -n1 adb pull
# delete from device
adb shell rm /sdcard/*.log
```

## Parse and Generate Results
