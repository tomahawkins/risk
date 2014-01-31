// An example of a RISK system with 3 partitions:
//
//   sensor -> controller -> actuator
//

#include <stdio.h>

#include "risk_api_sensor.h"
#include "risk_api_controller.h"
#include "risk_api_actuator.h"

// The sensor partition main entry point.
void sensor_main (void)
{
	word msg1[1] = { 1 };
	word msg2[2] = { 2, 3 };
	for (;;) {
		printf("Hi, I'm the sensor.  I'm sending these messages to the controller:    [ 1 ] [ 2 3 ]\n");
		sensor_to_controller_send_init();
		sensor_to_controller_send_msg(1, msg1);
		sensor_to_controller_send_msg(2, msg2);
		risk_yield();
	}
}

// The controller partition main entry point.
void controller_main (void)
{
	word i;
	word size;
	word msg[20];
	for (;;) {
		printf("Hi, I'm the controller.  I received these messages from the sensor:   ");
		controller_from_sensor_recv_msg(&size, msg);
		while (size) {
			printf("[");
			for (i = 0; i < size; i++)
				printf(" %d", (int) (msg[i]));
			printf(" ] ");
			controller_from_sensor_recv_msg(&size, msg);
		}
		printf("\n");
		printf("And I'm sending this message to the actuator:                         [ 1 2 3 4 ]\n");
		msg[0] = 1;
		msg[1] = 2;
		msg[2] = 3;
		msg[3] = 4;
		controller_to_actuator_send_init();
		controller_to_actuator_send_msg(4, msg);
		risk_yield();
	}
}

// The actuator partition main entry point.
void actuator_main (void)
{
	word i;
	word size;
	word msg[20];
	for (;;) {
		printf("Hi, I'm the actuator.  I received these messages from the controller: ");
		actuator_from_controller_recv_msg(&size, msg);
		while (size) {
			printf("[");
			for (i = 0; i < size; i++)
				printf(" %d", (int) (msg[i]));
			printf(" ] ");
			controller_from_sensor_recv_msg(&size, msg);
		}
		printf("\n");
		risk_yield();
	}
}

