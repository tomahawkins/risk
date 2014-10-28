# RISK: a Realtime Information-flow-control Separation Kernel

The goals of RISK are to produce a compile-time configurable 
separation kernel, simple enough to be verified automatically.
Partitions communicate via. message passing over unidirectional,
verifiable information flow control (IFC) channels.

# RISK Status

## Implementation Status

- The [partition specification](https://github.com/tomahawkins/risk/blob/master/RISK/Spec.hs)
  and the [kernel configuration](https://github.com/tomahawkins/risk/blob/master/RISK/Config.hs) modules are defined.
- The configurable [kernel](https://github.com/tomahawkins/risk/blob/master/RISK/Kernel.hs) is partially implemented.
  - A round-robin scheduler is running.
  - IPC is implemented and tested.  See the [example](https://github.com/tomahawkins/risk/blob/master/partitions.c).
  - Current limitations:
    - No timer or IO interrupts. 
    - No preemption.
    - No memory protections.

## Verification Status

- Successfully verified properties of the kernel:
  - Type Safety: Absence of runtime errors verified by [GIGL's use of Haskell types](https://github.com/tomahawkins/gigl/blob/master/Language/GIGL.hs).
  - [Termination](https://github.com/tomahawkins/risk/blob/master/RISK/Verify.hs):
    The kernel will never get stuck in an infinite loop.
    Verified by limitations of GIGL (i.e. no loops) and call graph analysis (i.e. no recursion).
  - [Return to Partition](https://github.com/tomahawkins/risk/blob/master/RISK/Verify.hs):
    The kernel will always yield to a user partition.
    Verified by program path enumeration.
  - [Valid Message Transfer](https://github.com/tomahawkins/risk/blob/master/RISK/Verify.hs):
    The kernel only transfers messages between specified partitions.
    Verified by extracting all TransferMessages intrinsics and comparing source and destination partitions to spec.
- Verification assumes:
  - A complete specification (which is far from it at the moment).
  - The trusted computing base:
    - Haskell compiler.
    - Verification analysis functions.
    - RISK's GIGL compiler.
    - RISK intrinsic implementations, e.g. TransferMessages, RestoreContext.
    - C compiler, assembler, hardware, etc.

# Random Ideas

- A language to specify the partitions, their execution rates, and the communication channels between them.
  - Partitions form a digraph.  Edges are message passing.
- DSL to implement kernel.  Verify security properties on DSL and verify refinement to assembly.
  - Generated assembly is annotated for disassembly back to source.
  - Target x86\_64 first.
- Maintainable verification.
  - With fully inline functions, each kernel call can be models as state transition relation.
  - Could be amenable to SMT.
- Properties:
  - Correctness:
    - All kernel API calls terminate and return to the user level.
    - Kernel can never enter an infinite loop.
    - Kernel can never crash or throw an exception.
    - Scheduling is correct.
  - IFC:
    - Any two partitions exhibit noninterference if they have no mutual communication channels.
    - Even stronger:
      - forall a b . ! (a communicatesWith b) => (a noninterfersWith b).
- Simple DSL and code gen to guarantee termination.
  - A set on non recursive, top-level functions.
  - Each function has no loops.
  - Or... No loops and no calls.  Each function is fully inlined.
    - At least no loops at DSL.  Some DSL primitives will need loops at assembly, e.g. memory copies.
- What is possible?
  - Dynamic partition creation?
  - Multi-processor support?

## Inter Process/Partition Communication (IPC)

Communication between partitions is done with synchronous message passing.
Each channel between partitions has two message buffers: one at the sending partition and one at the receiving partition.
The kernel does not buffer any messages, it merely transfers messages between the sending and receiving buffers.
Sending a message does not require an explicit system call.  Instead, if the thread calls yield, this tells the kernel
that the sending buffers are in a valid state to check and, if messages are available,  perform message transfers.

Messages are variable length.  Messages are formatted with the first word designated as the length 
of the message, followed by the message itself.

### Sending Message Buffers

Sending message buffers reside in the sending threads' memory space, not the kernel.

At any point in the buffer, a zero length field signifies
that there are no remaining messages in the buffer.
If the first word in the buffer is 0, the buffer contains no messages.

It is the responsibility of the sending thread to ensure the sending message buffers
are formatted correctly prior to calling 'yield'.
The kernel will only check for valid message lengths, e.g. length is greater than 0 and less than 
the remaining space in the buffer.

On kernel entry, if the kernel sees a yield signal from the active thread, it looks at its outgoing buffers
for any messages to transfer.  The kernel will transfer all messages in the buffer
until it encounters a zero message length field, it reaches the end of the buffer,
or it runs out of space in the associated receiving buffer.

### Receiving Message Buffers

Unlike sending message buffers, receiving message buffers are implemented as
circular FIFOs.  The FIFO has two pointers: a head, which points to the next unread message,
and a tail, which points to the first available free space in the buffer.  The 
head buffer is managed by the user thread, the tail is managed by the kernel.
(`head == tail` signifies the buffer is empty, not full.  Or we need an 
extra bool state element to determine full/empty.)

When transferring a message from a send buffer to a receive buffer, the kernel
will check if there is enough room in the buffer:

```c
room = message_length <= (head - tail) % buffer_length;
```

If there is not enough room in the receive buffer, the kernel will drop
all the remaining messages and no indication will be provided to the sender (no information backchannel).
The kernel could provide a running counter to indicate to the receiver when messages get dropped.

When the receiving thread is done with a message, it will advance the head pointer
to the next message:

```c
head = (head + *head) % buffer_length;
```

It is the responsibility of the receiving thread to correctly manage the receiving buffer and the head pointer.

### Alternative Messaging Scheme

Instead of separate buffers for each channel link between partitions, each parition gets one outgoing and
one incoming buffer.  The message format now includes a partition id field: (partition id : payload size : message payload)
When the kernel transfers messages, it replaces the target partition id with the senders' id.

A channel table managed by the kernel determines if sending a message between two partitions is allowed.

## Dynamic Partitions

Partitions that can create and delete subpartitions.

Under this proposal, partitions are not configured at compile time.  Instead, an initial root partition
is responsible for setting up the partition configuration.

Configuration passed to kernel to create new partition:

- Partition memory size and location.
- Incoming and outgoing buffer size.
- Allowable communication channels.
  - But if a cyclic channel is needed, all the partition ids are needed prior to creating any one partition.
  - So a call to the kernel is needed to establish a fresh channel id.  This
    is when the kernel registers the parent-child pair in the partition table.

Once a partition is created, the parent partition can not access the child partition's memory.

that can split their memory space to create new partitions and also can reclaim subpartitions.

### Kernel API

To make a request of the kernel, the partition and kernel communicate via a special message buffer
that resides in the partition memory space.  The partition places the request in the buffer,
then calls yield (also making sure the channel buffers are in a valid state).  The kernel
executes the request and places its response back into the buffer.

Kernel transaction types:

- Request fresh partition id.
  - Request:  command
  - Response: new-partition-id
  - Kernel returns a fresh id and logs entry in parent-child partition table.
- Enable channel pair.  Two partition ids are allowed to communicate with each other.
  - Request:  command src-id dest-id
  - Response: none
  - Kernel checks that the pair is allowable, i.e., that both ids are children of the active partition
    or that one is a child and the other is the active partition (parent).
- Disable channel pair.
  - Request:  command src-id dest-id
  - Response: none
- Set memory location and size of child partition.
  - Request:  command child-id location size
  - Response: none
- Set incoming and outgoing buffer sizes of child partition.
  - Request:  command child-id incoming-size outgoing-size
  - Response: none
- Enable partition (memory and channel buffers must have been specified).
  - Request:  command child-id
  - Response: none
- Disable partition.
- Delete partition.

Q: How does a parent reclaim the memory of a deleted child partition,
   especial if it is not on the parent's partition boundry?

### Kernel Data Structures

- Next partition id.
  - To allow recycling of old ids, this is captured as list of deleted partitions plus the next fresh id.
- Parent-child partition table.
- Allowed channel table (valid source to receiver).

# Links

- seL4.
  - [seL4](http://ssrg.nicta.com.au/projects/seL4/)
  - [seL4 News](http://microkerneldude.wordpress.com/2013/05/03/closing-the-gap-real-os-security-finally-in-reach/)
  - [seL4 Proof Maintenance](http://ssrg.nicta.com.au/projects/TS/maintenance.pml)
  - [Presentation](http://plosworkshop.org/2011/presentations/heiser.pdf)
- Other separation kernels.
  - [IntegrityOS](http://www.ghs.com/products/rtos/integrity.html)
  - [HiStar](http://www.scs.stanford.edu/histar/)
  - [Muen SK](http://muen.codelabs.ch/)
- Certification stuff.
  - [SKPP Revised](http://fm.csl.sri.com/LAW/2010/law2010-03-Levin-Nguyen-Irvine.pdf)
  - [Sunsetting SKPP](https://www.niap-ccevs.org/pp/pp.cfm?id=pp_skpp_hr_v1.03/&CFID=18625880&CFTOKEN=df14a452be6f0ace-5898D0A1-960A-B84F-7165A6A332503768)
- Tools.
  - [QEMU](http://wiki.qemu.org/Main_Page)
- x86.
  - [x86-64 ABI](http://www.x86-64.org/documentation/abi.pdf)


