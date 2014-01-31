// An example of a RISK system with 3 partitions:
//   sensor -> control -> actuator

#include <stdio.h>

#include "risk_lib.h"

void sensor_main (void)
{
	extern word sensor_to_control_send_buffer;
	word * buf = (word *) sensor_to_control_send_buffer;
	for (;;) {
		printf("Hi, I'm the sensor.\n");
		printf("I'm sending [1] and [2, 3] to the controller.\n");
		buf[0] = 1;
		buf[1] = 1;
		buf[2] = 2;
		buf[3] = 2;
		buf[4] = 3;
		buf[5] = 0;
		risk_yield();
	}
}

void control_main (void)
{
	extern word control_from_sensor_head_index;
	extern word control_from_sensor_recv_buffer;
	extern word control_from_sensor_tail_index;
	word * buf  = (word *) control_from_sensor_recv_buffer;
	word * head = (word *) control_from_sensor_head_index;
	word * tail = (word *) control_from_sensor_tail_index;
	for (;;) {
		printf("Hi, I'm the controller.\n");
		printf("I'm receiving the following from the sensor:\n");
		while ((*head & 0x7) != (*tail & 0x7)) {
		  printf("  buf[%x] = %d\n", (int) (*head & 0x7), (int) buf[*head & 0x7]);
		  *head = *head + 1;
		}
		risk_yield();
	}
}

void actuator_main (void)
{
	for (;;) {
		printf("Hi, I'm the actuator.\n");
		risk_yield();
	}
}

