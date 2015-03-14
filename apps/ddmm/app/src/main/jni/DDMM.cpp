#include <jni.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <time.h>
#include <linux/limits.h>
#include <android/log.h>

static const char* kModuleName = "DDMM";

static const char* kCapacityFile = "/sys/class/power_supply/battery/capacity";
static const char* kCurrentFile = "/sys/class/power_supply/battery/current_now";
static const char* kVoltageFile = "/sys/class/power_supply/battery/voltage_now";
static const char* kMaxVoltageFile =
    "/sys/class/power_supply/battery/voltage_max_design";
static const char* kMinVoltageFile =
    "/sys/class/power_supply/battery/voltage_min_design";
static const char* kTemperatureFile = "/sys/class/power_supply/battery/temp";

static const char* kWlanStatsRecvBytes =
    "/sys/class/net/wlan0/statistics/rx_bytes";
static const char* kWlanStatsRecvPackets =
    "/sys/class/net/wlan0/statistics/rx_packets";
static const char* kWlanStatsRecvDropped =
    "/sys/class/net/wlan0/statistics/rx_dropped";
static const char* kWlanStatsRecvErrors =
    "/sys/class/net/wlan0/statistics/rx_errors";

static const char* kWlanStatsSentBytes =
    "/sys/class/net/wlan0/statistics/tx_bytes";
static const char* kWlanStatsSentPackets =
    "/sys/class/net/wlan0/statistics/tx_packets";
static const char* kWlanStatsSentDropped =
    "/sys/class/net/wlan0/statistics/tx_dropped";
static const char* kWlanStatsSentErrors =
    "/sys/class/net/wlan0/statistics/tx_errors";

static const char* kCpuIsOnline[4] = {"/sys/devices/system/cpu/cpu0/online",
                                      "/sys/devices/system/cpu/cpu1/online",
                                      "/sys/devices/system/cpu/cpu2/online",
                                      "/sys/devices/system/cpu/cpu3/online"};

static const char* kCpuFrequency[4] = {
    "/sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq",
    "/sys/devices/system/cpu/cpu1/cpufreq/scaling_cur_freq",
    "/sys/devices/system/cpu/cpu2/cpufreq/scaling_cur_freq",
    "/sys/devices/system/cpu/cpu3/cpufreq/scaling_cur_freq"};

static const char* kCpuFreqInState[4] = {
    "/sys/devices/system/cpu/cpu0/cpufreq/stats/time_in_state",
    "/sys/devices/system/cpu/cpu1/cpufreq/stats/time_in_state",
    "/sys/devices/system/cpu/cpu2/cpufreq/stats/time_in_state",
    "/sys/devices/system/cpu/cpu3/cpufreq/stats/time_in_state"};

#define readIntFromFile(filename, buffer, length) \
  (int) readLongFromFile(filename, buffer, length)

#define DDMMService(functionName) \
  Java_br_ufpe_emilianofirmino_ddmm_service_DroidDMMService_##functionName

struct ProfileContext {
  bool isActive;
  char battery_logname[PATH_MAX];
  char network_logname[PATH_MAX];
  char processor_logname[PATH_MAX];

  ProfileContext() : isActive(false) {
    memset(battery_logname,   0, sizeof(char) * PATH_MAX);
    memset(network_logname,   0, sizeof(char) * PATH_MAX);
    memset(processor_logname, 0, sizeof(char) * PATH_MAX);
  }
};

int readLongFromFile(const char* filename, char* buffer, int length) {
  long value = -1;
  FILE* file = fopen(filename, "r");

  if (file != NULL && fgets(buffer, length, file) != NULL) {
    value = strtol(buffer, (char**)NULL, 10);
    fclose(file);
  }

  return value;
}

void* battery_monitor(void* ptr) {
  ProfileContext* ctx = (ProfileContext*)ptr;
  const int maxlen = 512;

  FILE* logfile = fopen(ctx->battery_logname, "w");
  char* logline = new char[maxlen + 1];

  if (logfile != NULL) {
    struct timeval begin;
    struct timeval now;
    struct timeval delta;

    struct tm datetime;

    const double maxVoltage =
        readIntFromFile(kMaxVoltageFile, logline, maxlen) / 1000000.0;
    const double minVoltage =
        readIntFromFile(kMinVoltageFile, logline, maxlen) / 1000000.0;

    gettimeofday(&begin, NULL);
    for (;;) {
      gettimeofday(&now, NULL);
      timersub(&now, &begin, &delta);

      int charge = readIntFromFile(kCapacityFile, logline, maxlen);
      double mcurrent = readIntFromFile(kCurrentFile, logline, maxlen) / 1000.0;
      double voltage =
          readIntFromFile(kVoltageFile, logline, maxlen) / 1000000.0;
      double temperature =
          readIntFromFile(kTemperatureFile, logline, maxlen) / 10.0;
      double mpower = voltage * mcurrent;

      datetime = *localtime(&now.tv_sec);
      datetime.tm_year += 1900;
      datetime.tm_mon += 1;

      int len =
          snprintf(logline, maxlen,
                   "%d/%d/%d-%d:%d:%d.%06ld "
                   "%ld.%06ld "
                   "%d %lf %lf %lf %lf %lf %lf\n",
                   datetime.tm_year, datetime.tm_mon, datetime.tm_mday,
                   datetime.tm_hour, datetime.tm_min, datetime.tm_sec,
                   now.tv_usec, delta.tv_sec, delta.tv_usec, charge, mcurrent,
                   voltage, maxVoltage, minVoltage, mpower, temperature);

      if (len > 0) {
#ifdef DEBUG
        __android_log_print(ANDROID_LOG_VERBOSE, kModuleName, "%s", logline);
#endif
        fwrite(logline, len, 1, logfile);
      }

      if (ctx->isActive) {
        usleep(100 * 1000);
      } else {
        break;
      }
    }

    fclose(logfile);
  } else {
    __android_log_print(ANDROID_LOG_ERROR, kModuleName, "fail to open %s",
                        ctx->battery_logname);
  }

  delete[] logline;
  return ctx;
}

void* network_monitor(void* ptr) {
  ProfileContext* ctx = (ProfileContext*)ptr;
  const int maxlen = 512;

  FILE* logfile = fopen(ctx->network_logname, "w");
  char* logline = new char[maxlen + 1];

  if (logfile != NULL) {
    struct timeval begin;
    struct timeval now;
    struct timeval delta;

    struct tm datetime;

    gettimeofday(&begin, NULL);
    for (;;) {
      gettimeofday(&now, NULL);
      timersub(&now, &begin, &delta);

      datetime = *localtime(&now.tv_sec);
      datetime.tm_year += 1900;
      datetime.tm_mon += 1;

      long rx_bytes = readLongFromFile(kWlanStatsRecvBytes, logline, maxlen);
      long rx_packets =
          readLongFromFile(kWlanStatsRecvPackets, logline, maxlen);
      long rx_dropped =
          readLongFromFile(kWlanStatsRecvDropped, logline, maxlen);
      long rx_errors = readLongFromFile(kWlanStatsRecvErrors, logline, maxlen);

      long tx_bytes = readLongFromFile(kWlanStatsSentBytes, logline, maxlen);
      long tx_packets =
          readLongFromFile(kWlanStatsSentPackets, logline, maxlen);
      long tx_dropped =
          readLongFromFile(kWlanStatsSentDropped, logline, maxlen);
      long tx_errors = readLongFromFile(kWlanStatsSentErrors, logline, maxlen);

      int len = snprintf(logline, maxlen,
                         "%d/%d/%d-%d:%d:%d.%06ld "
                         "%ld.%06ld "
                         "%ld %ld %ld %ld "
                         "%ld %ld %ld %ld\n",
                         datetime.tm_year, datetime.tm_mon, datetime.tm_mday,
                         datetime.tm_hour, datetime.tm_min, datetime.tm_sec,
                         now.tv_usec, delta.tv_sec, delta.tv_usec, rx_bytes,
                         rx_packets, rx_dropped, rx_errors, tx_bytes,
                         tx_packets, tx_dropped, tx_errors);

      if (len > 0) {
#ifdef DEBUG
        __android_log_print(ANDROID_LOG_VERBOSE, kModuleName, "%s", logline);
#endif
        fwrite(logline, len, 1, logfile);
      }

      if (ctx->isActive) {
        usleep(100 * 1000);
      } else {
        break;
      }
    }

    fclose(logfile);
  } else {
    __android_log_print(ANDROID_LOG_ERROR, kModuleName, "fail to open %s",
                        ctx->network_logname);
  }

  delete[] logline;
  return ctx;
}

void* processor_monitor(void* ptr) {
  ProfileContext* ctx = (ProfileContext*)ptr;
  const int maxlen = 1024;

  FILE* logfile = fopen(ctx->processor_logname, "w");
  char* logline = new char[maxlen + 1];

  if (logfile != NULL) {
    struct timeval begin;
    struct timeval now;
    struct timeval delta;

    struct tm datetime;

    gettimeofday(&begin, NULL);
    for (;;) {
      gettimeofday(&now, NULL);
      timersub(&now, &begin, &delta);

      datetime = *localtime(&now.tv_sec);
      datetime.tm_year += 1900;
      datetime.tm_mon += 1;

      FILE* cpu[4] = {NULL, NULL, NULL, NULL};
      bool cpu_is_on[4] = {NULL, NULL, NULL, NULL};
      long cpu_freq[4] = {0, 0, 0, 0};

      for (int i = 0; i < 4; i++) {
        FILE* cpu_is_on_file = fopen(kCpuIsOnline[i], "r");
        char c = fgetc(cpu_is_on_file);

        cpu_is_on[i] = c == '1';
        if (cpu_is_on[i]) {
          cpu[i] = fopen(kCpuFreqInState[i], "r");
          cpu_freq[i] = readLongFromFile(kCpuFrequency[i], logline, maxlen);
        }

        fclose(cpu_is_on_file);
      }

      int len = snprintf(logline, maxlen,
                         "%d/%d/%d-%d:%d:%d.%06ld "
                         "%ld.%06ld ",
                         datetime.tm_year, datetime.tm_mon, datetime.tm_mday,
                         datetime.tm_hour, datetime.tm_min, datetime.tm_sec,
                         now.tv_usec, delta.tv_sec, delta.tv_usec);

      len += snprintf(logline + len, maxlen - len, "%d %d %d %d ",
                      cpu[0] == NULL ? 0 : 1, cpu[1] == NULL ? 0 : 1,
                      cpu[2] == NULL ? 0 : 1, cpu[3] == NULL ? 0 : 1);

      len += snprintf(logline + len, maxlen - len, "%ld %ld %ld %ld ",
                      cpu_freq[0], cpu_freq[1], cpu_freq[2], cpu_freq[3]);

      for (int i = 0; i < 4; i++) {
        if (cpu[i] == NULL) continue;

        len += snprintf(logline + len, maxlen - len, "cpu%d: ", i);

        while (fgets(logline + len, maxlen - len, cpu[i]) != NULL) {
          len += strlen(logline + len);
          logline[len - 1] = ' ';
        }
        fclose(cpu[i]);
      }
      logline[len - 1] = '\n';
      logline[len] = '\0';

      if (len > 0) {
#ifdef DEBUG
        __android_log_print(ANDROID_LOG_VERBOSE, kModuleName, "%s", logline);
#endif
        fwrite(logline, len, 1, logfile);
      }

      if (ctx->isActive) {
        usleep(100 * 1000);
      } else {
        break;
      }
    }

    fclose(logfile);
  } else {
    __android_log_print(ANDROID_LOG_ERROR, kModuleName, "fail to open %s",
                        ctx->network_logname);
  }

  delete[] logline;
  return ctx;
}

static ProfileContext profile_context;
static pthread_t battery_monitor_thread = 0;
static pthread_t network_monitor_thread = 0;
static pthread_t processor_monitor_thread = 0;

extern "C" JNIEXPORT void JNICALL
DDMMService(startProfiler)(JNIEnv* env, jobject o) {
  if (battery_monitor_thread == 0 && network_monitor_thread == 0 &&
      processor_monitor_thread == 0) {
    time_t now = time(NULL);
    struct tm today = *localtime(&now);

    sprintf(profile_context.battery_logname,
            "/sdcard/battmon_%d-%d-%d_%d-%d-%d.log", today.tm_year + 1900,
            today.tm_mon + 1, today.tm_mday, today.tm_hour, today.tm_min,
            today.tm_sec);

    sprintf(profile_context.network_logname,
            "/sdcard/netmon_%d-%d-%d_%d-%d-%d.log", today.tm_year + 1900,
            today.tm_mon + 1, today.tm_mday, today.tm_hour, today.tm_min,
            today.tm_sec);

    sprintf(profile_context.processor_logname,
            "/sdcard/procmon_%d-%d-%d_%d-%d-%d.log", today.tm_year + 1900,
            today.tm_mon + 1, today.tm_mday, today.tm_hour, today.tm_min,
            today.tm_sec);

    profile_context.isActive = true;
    pthread_create(&battery_monitor_thread, NULL, battery_monitor,
                   &profile_context);
    pthread_create(&network_monitor_thread, NULL, network_monitor,
                   &profile_context);
    pthread_create(&processor_monitor_thread, NULL, processor_monitor,
                   &profile_context);
  } else {
    jclass klass = env->FindClass("java/lang/IllegalStateException");
    env->ThrowNew(klass, "profile already running");
  }
}

extern "C" JNIEXPORT void JNICALL
DDMMService(stopProfiler)(JNIEnv* env, jobject o) {
  if (battery_monitor_thread != 0 || network_monitor_thread != 0) {
    profile_context.isActive = false;

    pthread_join(battery_monitor_thread, (void**)NULL);
    pthread_join(network_monitor_thread, (void**)NULL);
    pthread_join(processor_monitor_thread, (void**)NULL);

    battery_monitor_thread = 0;
    network_monitor_thread = 0;
    processor_monitor_thread = 0;
  } else {
    jclass klass = env->FindClass("java/lang/IllegalStateException");
    env->ThrowNew(klass, "profile is not running");
  }
}

extern "C" JNIEXPORT bool JNICALL
DDMMService(isProfilerActive)(JNIEnv* env, jobject o) {
  return battery_monitor_thread != 0 || network_monitor_thread != 0 ||
         processor_monitor_thread != 0;
}
