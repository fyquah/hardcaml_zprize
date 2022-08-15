rm -f \
     device_trace_*.csv \
     emconfig.json \
     emulation_debug.log \
     *.protoinst \
     simulate.log \
     *.log \
     xrt.ini \
     *.exe

# Generated from xrt
rm -f summary.csv xrt.run_summary

# Generated from vivado sims
rm -rf .run *.wdb *.wcfg
