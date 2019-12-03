// Copyright (c) 2011, XMOS Ltd., All rights reserved
// This software is freely distributable under a derivative of the
// University of Illinois/NCSA Open Source License posted in
// LICENSE.txt and at <http://github.xcore.com/>

// SPI master and SPI slave loopback demo

#include <xs1.h>
#include <print.h>
#include "spi_master.h"
#include "spi_slave.h"

#define DEMO_RUNS 3

spi_master_interface spi_mif =
{
    XS1_CLKBLK_1,
    XS1_CLKBLK_2,
    XS1_PORT_1A,
    XS1_PORT_1E,
    XS1_PORT_1C
};
out port spi_ss = XS1_PORT_1G; // Single select line, not part of SPI master API

spi_slave_interface spi_sif =
{
    XS1_CLKBLK_3,
    XS1_PORT_1H,
    XS1_PORT_1B,
    XS1_PORT_1D,
    XS1_PORT_1F
};

void slave_select()
{
    spi_ss <: 0;
}

void slave_deselect()
{
    spi_ss <: 1;
}

#define BYTE_VALUE 0xA1
#define SHORT_VALUE 0xB1B2
#define INT_VALUE 0xC1C2C3C4
#define BUFFER_VALUE {0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8}
#define BUFFER_LENGTH 8

int check_buffer(unsigned char buffer[], int num_bytes)
{
    unsigned char tmp_buff[BUFFER_LENGTH] = BUFFER_VALUE;

    for (int i = 0; i < num_bytes; i++)
    {
        if (buffer[i] != tmp_buff[i])
        {
            return 1;
        }
    }

    return 0;
}

void check_data(unsigned char b, unsigned short s, unsigned int i, unsigned char buffer[], int num_bytes)
{
    int errorCount = 0;

    if (b != BYTE_VALUE)
    {
        errorCount++;
        printstr("master sent byte:\t");
        printhexln(BYTE_VALUE);
        printstr("master received byte:\t");
        printhexln(b);
    }

    if (s != SHORT_VALUE)
    {
        errorCount++;
        printstr("master sent short:\t");
        printhexln(SHORT_VALUE);
        printstr("master received short:\t");
        printhexln(s);
    }

    if (i != INT_VALUE)
    {
        errorCount++;
        printstr("master sent word:\t");
        printhexln(INT_VALUE);
        printstr("master received word:\t");
        printhexln(i);
    }

    if (check_buffer(buffer, num_bytes))
    {
        unsigned char tmp_buff[BUFFER_LENGTH] = BUFFER_VALUE;
        errorCount++;
        printstr("master sent buffer:\t{");
        for (int j = 0; j < BUFFER_LENGTH-1; j++)
        {
            printhex(tmp_buff[j]);
            printstr(", ");
        }
        printhex(tmp_buff[BUFFER_LENGTH-1]);
        printstrln("}");
        printstr("master received buffer:\t{");
        for (int j = 0; j < BUFFER_LENGTH-1; j++)
        {
            printhex(buffer[j]);
            printstr(", ");
        }
        printhex(buffer[BUFFER_LENGTH-1]);
        printstrln("}");
    }

    if (errorCount)
    {
        printint(errorCount);
        printstrln(" errors detected");
    }
    else
    {
        printstrln("All data returned from slave received correctly");
    }
}

void master_demo(spi_master_interface &spi_mif)
{
    unsigned char b = BYTE_VALUE;
    unsigned short s = SHORT_VALUE;
    unsigned int i = INT_VALUE;
    unsigned char buff[BUFFER_LENGTH] = BUFFER_VALUE;

    // Enable slave
    slave_select();

    // Write to slave
    spi_master_out_byte(spi_mif, b);
    spi_master_out_short(spi_mif, s);
    spi_master_out_word(spi_mif, i);
    spi_master_out_buffer(spi_mif, buff, BUFFER_LENGTH);

    // Read from slave
    b = spi_master_in_byte(spi_mif);
    s = spi_master_in_short(spi_mif);
    i = spi_master_in_word(spi_mif);
    spi_master_in_buffer(spi_mif, buff, BUFFER_LENGTH);

    // Disable slave
    slave_deselect();

    // Check the data received is correct
    check_data(b, s, i, buff, BUFFER_LENGTH);
}

void slave_demo(spi_slave_interface &spi_sif)
{
    unsigned char b;
    unsigned short s;
    unsigned int i;
    unsigned char buff[BUFFER_LENGTH];

    select
    {
        // Wait for slave to be enabled
        case spi_sif.ss when pinseq(1) :> void : // SS port inversion enabled, slave expected to be active low

            // Read from master
            b = spi_slave_in_byte(spi_sif);
            s = spi_slave_in_short(spi_sif);
            i = spi_slave_in_word(spi_sif);
            spi_slave_in_buffer(spi_sif, buff, BUFFER_LENGTH);

            // Write to master
            spi_slave_out_byte(spi_sif, b);
            spi_slave_out_short(spi_sif, s);
            spi_slave_out_word(spi_sif, i);
            spi_slave_out_buffer(spi_sif, buff, BUFFER_LENGTH);

            break;
    }
}

void run_master(spi_master_interface &spi_mif)
{
    spi_master_init(spi_mif, SPI_CLOCK_DIV);
    slave_deselect(); // Ensure slave select is in correct start state

    for (int i = 0; i < DEMO_RUNS; i++)
    {
        printstr("Demo run ");
        printintln(i+1);

        master_demo(spi_mif);
    }

    spi_master_shutdown(spi_mif);
}

void run_slave(spi_slave_interface &spi_sif)
{
    spi_slave_init(spi_sif);

    for (int i = 0; i < DEMO_RUNS; i++)
    {
        slave_demo(spi_sif);
    }

    spi_slave_shutdown(spi_sif);
}

int main(void)
{
    printstr("Running in SPI mode ");
    printintln(SPI_MODE);

    printstr("with SPI frequency ");
    printint((100/(2*SPI_CLOCK_DIV)));
    printstrln("MHz");

    printstr("for ");
    printint(DEMO_RUNS);
    printstrln(" demo runs");

    // Run SPI master and slave on seperate logical cores
    par
    {
        run_master(spi_mif);
        run_slave(spi_sif);
    }

    return 0;
}

