package edu.knowitall.tac2013.findSlotFillersApp


object CandidateType extends Enumeration{
  type CandidateType = Value
  val REGULAR, LINKED, COREF, INFERRED = Value
}