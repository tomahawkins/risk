# RISK: A Realtime Information flow control Separation Kernel

# Links

- seL4
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

# Ideas and Goals

- A language to specify the partitions, their execution rates, and the communication channels between them.
  - Partitions form a digraph.  Edges are message passing.
- DSL to implement kernel.  Verify security properties on DSL and verify refinement to assembly.
  - Generated assembly is annotated for disassembly back to source.
  - Target x86\_64 first.
- No timing channels for systems hard realtime threads that are only connected to other realtime threads.
- Maintainable verification.  Nearly fully automated.
  - With fully inline functions, each kernel call can be models as state transition relation.
  - Could be amenable to SMT.
- Properties:
  - Correctness:
    - All kernel API calls terminate and return to the user level.
    - Kernel can never enter an infinite loop.
    - Kernel can never crash or throw and exception.
    - Scheduling is correct (whatever that means).
  - IFC:
    - Any two partitions exibit noninterference if they have no mutual communication channels.
    - Even stronger:
      - forall a b . ! (a communicatesWith b) => (a noninterfersWith b).
- Simple code gen to guarantee termination.
  - A set on non recursive, top-level functions.
  - Each function has no loops.
  - Or... No loops and no calls.  Each function is fully inlined.
- What is possible?
  - Dynamic partition creation?
  - Multi-processor support?

