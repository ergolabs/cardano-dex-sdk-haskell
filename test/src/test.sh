#!/bin/sh

NAME=$1
TEST_HEX = $(NAME | xxd -pu)
echo ${TEST_HEX}