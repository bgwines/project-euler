IDIRS =
LDIRS =
LIBS =
STATIC_LIBS =

SRCS =
HEADERS =

TARGET = problem479
default : $(TARGET)

#################################
# FLINT
FLINT_VERSION = flint-2.3
FLINT_DIR = ./lib/flint/build
FLINT_LIB = $(FLINT_DIR)/lib/libflint.a
IDIRS += -I$(FLINT_DIR)/include/flint
STATIC_LIBS += $(FLINT_LIB)

MPFR_VERSION = mpfr-3.1.2
MPFR_DIR = ./lib/mpfr/build
MPFR_LIB = $(MPFR_DIR)/lib/libmpfr.a
IDIRS += -I$(MPFR_DIR)/include
STATIC_LIBS += $(MPFR_LIB)

MPIR_VERSION = mpir-2.3.1
MPIR_DIR = ./lib/mpir/build
MPIR_LIB = $(MPIR_DIR)/lib/libmpir.a
IDIRS += -I$(MPIR_DIR)/include
STATIC_LIBS += $(MPIR_LIB)
#################################

# Combine libs.
LIBS += $(STATIC_LIBS)

# Compiler info
CC = g++
CPPFLAGS = -Wall -Werror $(IDIRS) $(LDIRS)
ifdef GDB
CPPFLAGS += -O0 -ggdb
else
CPPFLAGS += -O3
endif

# Objects and executable
SRCS += $(TARGET).cpp
HEADERS +=
OBJS = $(SRCS:.cpp=.o)

$(TARGET) : $(FLINT_LIB) $(OBJS) $(HEADERS)
	$(CC) $(OBJS) $(CPPFLAGS) $(LIBS) -o $@
