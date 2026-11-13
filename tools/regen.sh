#!/bin/sh
# Regenerate the Haskell protocol bindings from protocol/*.xml.
set -e
cd "$(dirname "$0")/.."
IFACES=wl_display,wl_registry,wl_callback,wl_compositor,wl_shm,wl_shm_pool,wl_buffer,wl_surface,wl_region,wl_output
IFACES=$IFACES,zwlr_layer_shell_v1,zwlr_layer_surface_v1
IFACES=$IFACES,wp_viewporter,wp_viewport,wp_fractional_scale_manager_v1,wp_fractional_scale_v1
python3 tools/gen-protocol.py "$IFACES" protocol/*.xml > src/Bgmonad/Wayland/Protocol.hs
