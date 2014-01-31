// An example of a RISK system with 3 partitions:
//   sensor -> control -> actuator

#include <stdio.h>

#include "risk_api_sensor.h"
#include "risk_api_control.h"
#include "risk_api_actuator.h"

void sensor_main (void)
{
	extern word sensor_to_control_send_buffer;
	word * buf = (word *) sensor_to_control_send_buffer;
	for (;;) {
		printf("Hi, I'm the sensor.  I'm sending these messages to the controller:  [ 1 ] [ 2 3 ]\n");
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
	word i;
	word size;
	word msg[20];
	for (;;) {
		printf("Hi, I'm the controller.  I received these messages from the sensor: ");
		control_from_sensor_recv_msg(&size, msg);
		while (size) {
			printf("[");
			for (i = 0; i < size; i++)
				printf(" %d", (int) (msg[i]));
			printf(" ] ");
			control_from_sensor_recv_msg(&size, msg);
		}
		printf("\n");
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

