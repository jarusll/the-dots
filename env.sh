#!/bin/bash

# start charging when below 75
START_CHARGE_THRESH_BAT0=75
# stop charging when above 75
STOP_CHARGE_THRESH_BAT0=80

# same as above but for comptability reasons
START_CHARGE_THRESH_BAT1=75
STOP_CHARGE_THRESH_BAT1=80

# let it charge fully till 100 for calibration and then with above limits
RESTORE_THRESHOLDS_ON_BAT=1

# enable battery care for non thinkpads
NATACPI=1

# keep charge threshold values for non thinkpads
START_CHARGE_THRESH_BAT0=0  # dummy value
STOP_CHARGE_THRESH_BAT0=1

# disable continuous full functioning report
NMI_WATCHDOG=0

# wifi power savings
WIFI_PWR_ON_AC=off
WIFI_PWR_ON_BAT=on

# power profiles
PLATFORM_PROFILE_ON_AC=performance
PLATFORM_PROFILE_ON_BAT=low-power

# memory sleep profiles
MEM_SLEEP_ON_AC=s2idle
MEM_SLEEP_ON_BAT=deep

# operation mode for cpu, defined by governor in next section
CPU_DRIVER_OPMODE_ON_AC=passive
CPU_DRIVER_OPMODE_ON_BAT=passive

# powersave governor profile
CPU_SCALING_GOVERNOR_ON_AC=powersave
CPU_SCALING_GOVERNOR_ON_BAT=powersave

# energy profile for cpu
CPU_ENERGY_PERF_POLICY_ON_BAT=power

# disable cpu boost on battery
CPU_BOOST_ON_BAT=0
