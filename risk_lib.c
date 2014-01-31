// RISK Library.

#include <stdio.h>

#include "risk_lib.h"

// Copies data from a linear buffer to a circular buffer.
static void risk_copy
  ( word n                   // Number of words to transfer.
  , const word * src_buffer  // Source buffer.
  , word * src_index         // Source buffer starting index.  Index is incremented for every word copied.
  , word * dst_buffer        // Destination buffer.
  , word * dst_index         // Destination buffer starting index.
  , word dst_mask            // Destination index mask.
  )
{
	word final = * src_index + n;
	while (* src_index < final) {
		dst_buffer[* dst_index & dst_mask] = src_buffer[* src_index];
		* src_index = * src_index + 1;
		* dst_index = * dst_index + 1;
	}
}

// Space remaining in a circular buffer.
static word risk_recv_free_space (word mask, word head, word tail)
{
	return (head - tail - 1) & mask;  // When head == tail, this implies the buffer is empty, not full.
}

// Size of next message in send buffer.  0 if no messages available.
static word risk_size_of_next_send_msg (word buffer_size, word index, const word * buffer)
{
	if (index >= buffer_size || buffer[index] > buffer_size - index)  // Invalid index or invalid message size.
		return 0;
	else
		return buffer[index];
}

// Transfer messages on a channel between a send and recv buffer.
void risk_transfer_messages
  ( word         send_buffer_size
  , const word * send_buffer
  , word         recv_mask
  , word *       recv_buffer
  , word         recv_head
  , word *       recv_tail
  )
{
	word send_index = 0;
	word msg_size;
	msg_size = risk_size_of_next_send_msg(send_buffer_size, send_index, send_buffer);
	while (msg_size > 0 && msg_size + 1 < risk_recv_free_space(recv_mask, recv_head, *recv_tail)) {
		risk_copy(msg_size + 1, send_buffer, & send_index, recv_buffer, recv_tail, recv_mask);
		msg_size = risk_size_of_next_send_msg(send_buffer_size, send_index, send_buffer);
	}
}

// Yields control back to kernel.
void risk_yield (void)
{
	void risk_entry (void);
	risk_entry();
}

