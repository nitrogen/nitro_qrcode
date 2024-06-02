# QR Code Encoder

## Modified for use with the Nitrogen Web Framework

The primary modification has been in adding support for SVG generation.

The `nqr` prefix stands for "Nitrogen QR" and is there to disambiguate it from
the [original qrcode library](https://github.com/komone/qrcode) by
[Steve Davis](https://github.com/komone).

The full commit/fork history can be viewed in the [Github Network
View](https://github.com/komone/qrcode/network).

## Inclusion

Add this to your projects by adding this to your rebar.config:

```erlang
{deps, [
    nitro_qrcode
]}.
```


## Original Documentation

Reference used was ISO/IEC 18004, 1st Edition (2000)

This implementation is informed by my specific needs, i.e. to provide
two-factor authentication for mobile phones running Google Authenticator.

+ "Byte" mode only (don't need e.g. numeric mode or kanji mode).
+ Encode only (no detection/decode).
+ Basic supporting library functions provided (HOTP, PNG image functions) to allow full-cyle demo.


# Demo

1. Download repo and compile with `make` or `erl -make`
2. Install Google Authenticator App on your mobile:
	+ iPhone:  http://itunes.apple.com/us/app/google-authenticator/id388497605?mt=8
	+ Android: https://market.android.com/details?id=com.google.android.apps.authenticator
3. Run demo: `nqr_demo:run().`
4. Open the generated `qrcode.png` file
5. Scan the qrcode into the phone.
6. Ensure server clock is correct.
7. The value of `nqr_demo:totp()` should show the same passcode as the phone.
8. Handle PINs/logins for the second part of the "two factor" according to your application design.

NOTE: This documentation is rather basic as this was open-sourced by specific request!
