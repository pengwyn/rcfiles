#!/bin/zsh

function getend {
	awk 'END { print $0 }'
}

function getendcheck {
	out=$(getend)
	[[ $out = "FAIL" ]] && return 1
	return 0
}

id=$(wpa_cli add_network | getend)

name=$1
psk=$2

wpa_cli set_network $id ssid "\"$name\"" | getendcheck || (echo "Set SSID failed" ; exit 1)

if [[ -n $psk ]] ; then
	wpa_cli set_network $id psk "\"$psk\"" | getendcheck || (echo "Set PSK failed" ; exit 1)
else
	wpa_cli set_network $id key_mgmt NONE | getendcheck || (echo "Set no key management failed" ; exit 1)
fi

wpa_cli enable_network $id | getendcheck || (echo "Enable network failed" ; exit 1)
