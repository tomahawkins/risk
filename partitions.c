#include <stdio.h>

#include "risk_lib.h"

extern word sensor_1_to_sensor_voting_send_buffer;

void sensor_1_main (void)
{
	word * buf = (word *) sensor_1_to_sensor_voting_send_buffer;
	for (;;) {
		printf("Hi, I'm sensor_1.\n");
		printf("I'm sending [1] and [2, 3] to sensor_voting.\n");
		buf[0] = 1;
		buf[1] = 1;
		buf[2] = 2;
		buf[3] = 2;
		buf[4] = 3;
		buf[5] = 0;
		risk_yield();
	}
}

void sensor_2_main      (void) { for (;;) { printf("Hi, I'm sensor_2.\n"     ); risk_yield(); } }
void sensor_3_main      (void) { for (;;) { printf("Hi, I'm sensor_3.\n"     ); risk_yield(); } }

extern word sensor_voting_from_sensor_1_head_index;
extern word sensor_voting_from_sensor_1_recv_buffer;
extern word sensor_voting_from_sensor_1_tail_index;

void sensor_voting_main (void)
{
	word * buf  = (word *) sensor_voting_from_sensor_1_recv_buffer;
	word * head = (word *) sensor_voting_from_sensor_1_head_index;
	word * tail = (word *) sensor_voting_from_sensor_1_tail_index;
	for (;;) {
		printf("Hi, I'm sensor_voting.\n");
		printf("I'm receiving the following from sensor_1:\n");
		while ((*head & 0x7) != (*tail & 0x7)) {
		  printf("  buf[%x] = %d\n", (int) (*head & 0x7), (int) buf[*head & 0x7]);
		  *head = *head + 1;
		}
		risk_yield();
	}
}

void control_law_1_main (void) { for (;;) { printf("Hi, I'm control_law_1.\n"); risk_yield(); } }
void control_law_2_main (void) { for (;;) { printf("Hi, I'm control_law_2.\n"); risk_yield(); } }
void actuator_1_main    (void) { for (;;) { printf("Hi, I'm actuator_1.\n"   ); risk_yield(); } }
void actuator_2_main    (void) { for (;;) { printf("Hi, I'm actuator_2.\n"   ); risk_yield(); } }
void actuator_3_main    (void) { for (;;) { printf("Hi, I'm actuator_3.\n"   ); risk_yield(); } }

