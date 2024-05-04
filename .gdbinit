## COPY THIS TO /ETC/GDBINIT TO PERSIST

# open local .gdbinit by default
set auto-load local-gdbinit on

# automatically downlaod debug info entities
set debuginfod enabled on

tui enable

# only src 
layout src

# Set command history to be saved between sessions
set history save on

# Disable pagination for long outputs
set pagination off

# Enable pretty printing for better display of complex data structures
set print pretty on

# disable autoload protection
set auto-load safe-path /

# dont ask
set confirm off
