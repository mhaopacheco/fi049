*--------------------------------------------------------------------
*Nombre programa  :  ZFIC_049_POST_DOCS
*Tipo programa    :  Conversion
*Descripción      :  POsting Documents
*
*Especificaciones :  Y. Mauricio Pacheco P. (Consultor funcional WF)
*                 :  MQA Bussines Consulting (Colombia - s.a.)
*
*Fecha            :  07.04.2017 15:16:56
*Autor            :  Y. Mauricio Pacheco P. (Consultor SAP Netweaver Abap)
*                    E-mail: mauricio.pacheco@mqa-bc.com - maopacheco@gmail.com
*                    MQA Bussines Consulting (Colombia - s.a.)
*--------------------------------------------------------------------
* Empresa         :  CCB
*--------------------------------------------------------------------
*--------------------------------------------------------------------
*            l o g.   d e   m o d i f i c a c i o n e s
*--------------------------------------------------------------------
*    Fecha               Programador           Descripcion
*--------------------------------------------------------------------
* 07.04.2017 15:16:58    Mauricio Pacheco      Desarrollo inicial
*--------------------------------------------------------------------
* Comentarios y Observaciones acerca del programa:
* 07.04.2017 15:16:58 : <modificación critica sobre el desarrollo>
*--------------------------------------------------------------------

  INCLUDE zfic_049_post_docs_top. " Data Definition

  INCLUDE zfic_049_post_docs_e01. " Events
  INCLUDE zfic_049_post_docs_f01. " Soubroutines



*  INCLUDE ZFIC_049_POST_DOCS_i01. " PAI Modules
*  INCLUDE ZFIC_049_POST_DOCS_o01 . " PBO Modules

INCLUDE zfic_049_post_docs_f02.
