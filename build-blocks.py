#!/usr/bin/env python3

import getopt, sys, io
import binascii

def usage():
    print("""
Usage : build-blocks.py [OPTIONS] <source>

Options :
-f  Source as file
-o  Output format (raw,hex,base64), default : hex
""")
    sys.exit(2)

def output(block, format):
    if format == "raw":
        print(block.decode('unicode_escape'))
    elif format == "hex":
        print(binascii.b2a_hex(block).decode())
    elif format == "base64":
        print(binascii.b2a_base64(block).decode())

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:],"o:fh",["file", "output", "help"])
    except getopt.GetoptError as error:
        print(error)
        usage()
    source = ' '.join(args).encode()
    isFile = False
    out = "hex"
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            usage()
        elif opt in ("-f", "--file"):
            isFile = True
        elif opt in ("-o", "--output"):
            if arg in ("raw", "hex", "base64"):
                out = arg
            else:
                usage()
        else:
            usage()

    reader = open(source, "rb") if isFile else io.BytesIO(source)
    sourcelen = 0
    block = b''

    while True:
        block = reader.read(64)
        if len(block) < 64:
            sourcelen += len(block)
            break
        sourcelen += 64
        output(block, out)

    block += b'\x80'
    if len(block) >= 57:
        block += bytes(64-len(block))
        output(block, out)
        block = b''
    
    block += bytes(57-len(block))
    blocklen = bytes([((sourcelen & 0x1F) << 3) & 0xFF])
    sourcelen >>= 5
    for i in range(1, 7):
        blocklen += bytes([sourcelen & 0xFF])
        sourcelen >>= 8
    block += blocklen[::-1]
    output(block, out)

    reader.close()

if __name__ == "__main__":
    main()