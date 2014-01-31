// RISK Library.

#ifdef __cplusplus
extern "C" {
#endif

#ifdef RISK_WORD
#else
#define RISK_WORD
typedef unsigned long long word;
#endif

// Transfer messages on a channel between a send and recv buffer.
void risk_transfer_messages
  ( word         send_buffer_size
  , const word * send_buffer
  , word         recv_mask
  , word *       recv_buffer
  , word         recv_head
  , word *       recv_tail
  );

// Yields control back to kernel.  Yield also signals that the all
// outgoing (send) buffers are ready for transmission.
void risk_yield (void);

// Gets a message from a receive buffer.  Returns false if no message is available.  Advances the head pointer.
word risk_recv_msg (word mask, word * head, word tail, const word * buffer, word * msg);

#ifdef __cplusplus
}
#endif

