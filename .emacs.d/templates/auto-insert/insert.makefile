CC=gcc
CPPFLAGS=-Wall -O2 -g -DDEBUG
LDFLAGS=

OBJS=main.o

TARGET=main

%.d: %.c
	@set -e; rm -f $@; \
	$(CC) -MM -MT $*.o $(CPPFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

%.d: %.cpp
	@set -e; rm -f $@; \
	$(CC) -MM -MT $*.o $(CPPFLAGS) $< > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$


all: $(TARGET)

-include $(OBJS:.o=.d)

$(TARGET): $(OBJS)
	$(CC) -o $@ $(CPPFLAGS) $(LDFLAGS) $^
	strip -s $@

clean:
	-rm -f $(TARGET) $(OBJS) $(OBJS:.o=.d)
