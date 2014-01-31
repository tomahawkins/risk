// An example of a RISK system with 3 partitions:
//   sensor -> control -> actuator

#include <stdio.h>

#include "risk_api_sensor.h"
#include "risk_api_control.h"
#include "risk_api_actuator.h"

void sensor_main (void)
{
	word msg1[1] = { 1 };
	word msg2[2] = { 2, 3 };
	for (;;) {
		printf("Hi, I'm the sensor.  I'm sending these messages to the controller:  [ 1 ] [ 2 3 ]\n");
		sensor_to_control_send_clr();
		sensor_to_control_send_msg(1, msg1);
		sensor_to_control_send_msg(2, msg2);
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

