# MOD Player: Creates a table which allows the asm code to dynamically oversample
import os
import math

EXPORT_PATH = "D:\\Computer\\Atari\\Sources\\Ressources\\"
FRQ_FILE = "FrqTable"

AMIGA_FRQ = 7093789.2
AMIGA_DEFAULT_PERIOD = 428
AMIGA_PERIODS = (856, 808, 762, 720, 678, 640, 604, 570, 538, 508, 480, 453,
                 428, 404, 381, 360, 339, 320, 302, 285, 269, 254, 240, 226,
                 214, 202, 190, 180, 170, 160, 151, 143, 135, 127, 120, 113)

REPLAY_FRQ = (6258, 12517, 25033, 50066)
MP_BUFF_SLICE = (128, 256, 512, 1024)


def my_main():
    # Inits
    debug = 0

    # For debug, display the frequencies corresponding to the periods
    for i_period, period in enumerate(AMIGA_PERIODS):
        print(period, get_amiga_frq(period))

    # Create file
    print("Generating Frequency table file")
    with open(EXPORT_PATH + FRQ_FILE + ".S", "w") as tf:
        tf.write("; Tables to manage frequencies shift\n")
        for i_frq, frq in enumerate(REPLAY_FRQ):
            tf.write("; Replay frequency = " + str(frq) + "\n")
            # Create table of mapping
            tf.write("mp_period_to_frq_" + str(frq) + ":\n")
            tf.write("\tdc.l ")
            for i_period, period in enumerate(AMIGA_PERIODS):
                if i_period != 0:
                    tf.write(",")
                tf.write("mp_" + str(frq) + "_" + str(period))
            tf.write("\n\n")

            # The len of the buffer to fill at each VBL
            slice_len = MP_BUFF_SLICE[i_frq]
            buff_period = 1 / frq

            if i_frq == 1:
                debug = 1
            else:
                debug = 0

            # Now compute the tables for each period
            for i_period, period in enumerate(AMIGA_PERIODS):
                tf.write("mp_" + str(frq) + "_" + str(period)+":\t dc.w ")
                snd_frq = get_amiga_frq(period)
                snd_period = 1 / snd_frq

                if debug:
                    print("Period:", period, snd_frq, snd_period)
                    print("Pos:", end="")
                snd_pos_prev = 0
                for i_buff in range(0, slice_len):
                    buff_pos = i_buff * buff_period
                    snd_pos = int(round(buff_pos / snd_period, 1))
                    delta_pos = snd_pos - snd_pos_prev
                    if debug:
                        print(snd_pos, "(" + str(delta_pos)+") ", end="")

                    if i_buff != 0:
                        if i_buff != 1:
                            tf.write(",")
                        tf.write(str(delta_pos))

                    snd_pos_prev = snd_pos

                if debug:
                    print()
                tf.write("\n")
            tf.write("\n")
        tf.close()
        print("Frq Table asm file saved in", os.path.getsize(EXPORT_PATH + FRQ_FILE + ".S"), "bytes")


def get_amiga_frq(period):
    return int(AMIGA_FRQ / (period * 2))


# Call MAIN
my_main()
