# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)])." -s init stop)
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)])." -s init stop)
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)])." -s init stop)

PROFILE ?= default
C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT_DIR = $(BASEDIR)/priv/$(PROFILE)
LZ4_NIF_OUTPUT ?= $(C_SRC_OUTPUT_DIR)/lz4_nif.so
LZ4FRAME_NIF_OUTPUT ?= $(C_SRC_OUTPUT_DIR)/lz4_frame_nif.so
C_SRC_OUTPUT = $(LZ4_NIF_OUTPUT) $(LZ4FRAME_NIF_OUTPUT)
# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall
	LDFLAGS ?= -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -std=c99 -finline-functions -Wall
endif

ifeq ($(PROFILE), test)
	CFLAGS += -Og -g3 -fno-inline-functions -fno-omit-frame-pointer
else
	CFLAGS += -O3 -finline-functions
endif

CFLAGS += -fpic -fPIC

OBJ_CFLAGS = $(CFLAGS)
NIF_CFLAGS = $(CFLAGS) -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -Wmissing-prototypes

LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -lei
LDFLAGS += -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*.c" -and ! -name "*_nif.c" \))
OBJDIR = $(CURDIR)/$(PROFILE)
OBJECTS = $(addprefix $(OBJDIR)/, $(addsuffix .o, $(basename $(notdir $(SOURCES)))))

.DEFAULT := all
all: $(C_SRC_OUTPUT)

$(LZ4_NIF_OUTPUT): lz4_nif.c $(OBJECTS)
	@mkdir -p $(C_SRC_OUTPUT_DIR)
	$(link_verbose) $(CC) lz4_nif.c $(OBJECTS) $(NIF_CFLAGS) $(LDFLAGS) $(LDLIBS) -o $(LZ4_NIF_OUTPUT)

$(LZ4FRAME_NIF_OUTPUT): lz4frame_nif.c $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) lz4frame_nif.c $(OBJECTS) $(NIF_CFLAGS) $(LDFLAGS) $(LDLIBS) -o $(LZ4FRAME_NIF_OUTPUT)

$(OBJDIR)/%.o: %.c
	@mkdir -p $(OBJDIR)
	$(c_verbose) $(CC) $(OBJ_CFLAGS) -c $< -o $@

clean:
	rm -f $(C_SRC_OUTPUT) $(OBJECTS)
