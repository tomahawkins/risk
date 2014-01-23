# RISK: a Realtime Information-flow-control Separation Kernel

The goals of RISK are to produce a compile-time configurable 
separation kernel, simple enough to be verified automatically.
Partitions communicate via. message passing over unidirectional,
verifiable information flow control (IFC) channels.
And for realtime partitions, an attempt will be made
to minimize covert timing channels.

# RISK Status

## Implementation

- The [partition specification](https://github.com/tomahawkins/risk/blob/master/RISK/Spec.hs)
  and the [kernel configuration](https://github.com/tomahawkins/risk/blob/master/RISK/Config.hs) modules are defined.
- The syntax of the implementation DSL, [GIGL](https://github.com/tomahawkins/gigl), is operational.
  - Currently GIGL has no simulator or backend implementation targets. 
- An abstract [kernel](https://github.com/tomahawkins/risk/blob/master/RISK/Kernel.hs) is implemented,
  with many high level operations (e.g. interrupt handlers, IPC, etc.) stubbed off as intrinsics.
- A round-robin scheduler is implemented.
- A kernel simulator is running in C with the provided flight control [example](https://github.com/tomahawkins/risk/blob/master/partitions.c).
  Limitations:
  - Supports non-preemptive partitions only, i.e. partitions must called yield.
  - Currently lacks IPC.
  - Memory partitions are not enforced.

## Verification

- Successfully verified properties of the kernel:
  - Type Safety: Absense of runtime errors verified by [GIGL's use of Haskell types](https://github.com/tomahawkins/gigl/blob/master/Language/GIGL.hs).
  - [Termination](https://github.com/tomahawkins/risk/blob/master/RISK/Verify.hs): The kernel will always yield to a user partition,
    verified by code reachability analysis.
- GIGL can target [ACL2](http://www.cs.utexas.edu/~moore/acl2/), though ACL2 is not currently used in verification.

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

# Virtual Memory

Or not...  Is it possible just to give the threads restricted DMA?

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


