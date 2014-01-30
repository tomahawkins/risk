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

#ifdef __cplusplus
}
#endif

